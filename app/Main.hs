import QualityMeasures
import Parser
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.Writer

data Result = NEI | NonDenom | Code Code

main = do
       [fqms, fpats, qmid, patid] <- fmap parseArgs getArgs
       txtqms <- readFile fqms
       txtpats <- readFile fpats
       let qms = parseQualityMeasures fqms txtqms
           qm = case lookup qmid qms of
                  Just x -> x
                  Nothing -> error "Quality measure id not found."
           (n,d) = qm
           pats = parsePatients fpats txtpats
           pat = case lookup patid pats of
                  Just x -> x
                  Nothing -> error "Patient id not found."
       (result, tests, pat') <- getResult d n pat
       case result of
         NEI -> putStrLn "Not enough information provided to determine quality code."
         NonDenom -> putStrLn "The patient does not fit the denominator criteria."
         Code code -> putStrLn $ "The patient quality code is " ++ code
       putStrLn "The following propositions were evaluated:"
       mapM_ print tests
       putStrLn "The patient information would be updated to:"
       print pat'


getResult :: Cond -> Numerator -> Patient -> IO (Result, [Test], Patient)
getResult c n p = do
  case evalCond c p of
    (Left c', ts) -> do
      (mb, ts', p') <- askCond c' p
      case mb of
         Nothing -> return (NEI, ts++ts', p')
         Just True -> do
           code <- askCode n
           return (Code code, ts++ts', p')
         Just False -> return (NonDenom, ts++ts', p')
    (Right True, ts) -> do
      code <- askCode n
      return (Code code, ts, p)
    (Right False, ts) -> return (NonDenom, ts, p)
  
askCode :: Numerator -> IO Code
askCode assocs = do
  putStrLn "Choose numerator:"
  forM_ (zip [1..] $ map fst assocs) $ \(i, msg) -> do
    putStrLn $ show i ++ ". " ++ msg
  putStrLn "Enter number:"
  k <- fmap read getLine
  return $ snd (assocs !! (k-1))

askCond :: Cond -> Patient -> IO (Maybe Bool, [Test], Patient)
askCond cond pt = do ((a,b),c) <- runWriterT (askCond' cond pt)
                  return (a,c,b)
  where
    askCond' :: Cond -> Patient -> WriterT [Test] IO (Maybe Bool, Patient)
    askCond' (Prop p) pat = do
      (mb, pat') <- lift $ askProp p pat
      case mb of
        Nothing -> return (Nothing, pat')
        Just b -> do
          tell [(b, p)]
          return (mb, pat')
    askCond' (Not c) pat = do
      (mb, pat') <- askCond' c pat
      case mb of
        Nothing -> return (Nothing, pat')
        Just b -> return (Just (not b), pat')
    askCond' (And []) pat = return (Just True, pat)
    askCond' (And (c:cs)) pat = do
      (mb, pat') <- askCond' c pat
      case mb of
        Nothing -> return (Nothing, pat')
        Just True -> askCond' (And cs) pat'
        Just False -> return (Just False, pat')
    askCond' (Or []) pat = return (Just False, pat)
    askCond' (Or (c:cs)) pat = do
      (mb, pat') <- askCond' c pat
      case mb of
        Nothing -> return (Nothing, pat')
        Just True -> return (Just True, pat')
        Just False -> askCond' (Or cs) pat'

askProp :: Prop -> Patient -> IO (Maybe Bool, Patient)
askProp (AgeGEQ t) p = do
  putStrLn "Enter patient's age:"
  s <- getLine
  if null s
    then return (Nothing, p)
    else do
      let n = read s
          p' = p { age = Just n }
      return (evalProp (AgeGEQ t) p', p')
askProp (CPT desc codes) p = do
  putStrLn "No procedure on record satisfying"
  putStrLn $ "Description = " ++ desc
  putStrLn $ "Member of = " ++ show codes
  putStrLn "Enter procedure code or leave blank:"
  code <- getLine
  if null code
    then return (Nothing, p)
    else let p' = appendCPT p code
          in return (evalProp (CPT desc codes) p', p')
askProp (ICD10 desc codes) p = do
  putStrLn "No diagnosis found satisfying"
  putStrLn $ "Description = " ++ desc
  putStrLn $ "Codes = " ++ show codes
  putStrLn $ "Enter diagnosis code or leave blank:"
  code <- getLine
  if null code
    then return (Nothing, p)
    else let p' = appendICD10 p code
          in return (evalProp (ICD10 desc codes) p', p')
askProp (Q prompt) p = do
  putStrLn "Answer the following with (y,n), or leave blank if unknown:"
  putStrLn prompt
  s <- getLine
  case s of
    "" -> return (Nothing, p)
    "y" -> return (Just True, p)
    "n" -> return (Just False, p)

appendCPT :: Patient -> Code -> Patient
appendCPT p c = case procs p of
  Complete cs -> p { procs = Complete (c:cs) }
  Partial cs -> p { procs = Partial (c:cs) }

appendICD10 :: Patient -> Code -> Patient
appendICD10 p c = case diags p of
  Complete cs -> p { diags = Complete (c:cs) }
  Partial cs -> p { diags = Partial (c:cs) }

parseArgs :: [String] -> [String]
parseArgs args | len==1 = ["qualityData.txt", "patientData.txt", "044"] ++ args
               | len==2 = ["qualityData.txt", "patientData.txt"] ++ args
               | len==4 = args
               | otherwise = error "Wrong number of arguments."
  where len = length args

