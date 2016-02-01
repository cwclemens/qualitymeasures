module Parser (parseCond, parseQualityMeasures, parsePatients, Numerator, NumeratorOption) where
import QualityMeasures
import Text.Parsec
import Control.Applicative hiding (many, (<|>))

-- datatypes

type Parser a = Parsec String () a
type NumeratorOption = (String, Code)
type Numerator = [NumeratorOption]

-- public methods

parseCond :: String -> Cond
parseCond s = case parse cond "" s of
  Left err -> error (show err)
  Right c -> c

parseQualityMeasures :: String -> String -> [(String, (Numerator, Cond))]
parseQualityMeasures filename s = case parse qualityMeasures filename s of
  Left err -> error (show err)
  Right c -> c

parsePatients :: String -> String -> [(String, Patient)]
parsePatients filename s = case parse patients filename s of
  Left err -> error (show err)
  Right c -> c

-- parsers

nat :: Parser Int
nat = read <$> many1 digit

code :: Parser Code
code = many1 $ alphaNum <|> oneOf "-."

quotes :: Parser String
quotes = between (char '\"') (char '\"') (many (noneOf "\""))

codes :: Parser [Code]
codes = between (char '[') (char ']') (code `sepBy` (char ',' >> spaces))

field :: String -> (a -> b) -> Parser a -> Parser b
field lbl f px = do
                 string lbl
                 spaces
                 x <- px
                 return $ f x
                 
field2 :: String -> (a -> b -> c) -> Parser a -> Parser b -> Parser c
field2 lbl f px py = do
               string lbl
               spaces
               x <- px
               spaces
               y <- py
               return $ f x y

patients :: Parser [(String, Patient)]
patients = spaces *> many patient <* eof
  where
    patient :: Parser (String, Patient)
    patient = do
      key <- many1 digit
      spaces
      fns <- between (char '{') (char '}') $ (spaces *> many patAttrib)
      let pat = Patient Nothing (Partial []) (Partial [])
          pat' = foldr (.) id fns $ pat
      spaces
      return (key, pat')

    patAttrib :: Parser (Patient -> Patient)
    patAttrib = (patAge <|> patProcs <|> patDiags) <* spaces

    patAge :: Parser (Patient -> Patient)
    patAge = do
      string "age="
      nullAge <|> natAge
  
    natAge :: Parser (Patient -> Patient)
    natAge = do
      n <- nat
      return $ \p -> p { age = Just n }

    nullAge :: Parser (Patient -> Patient)
    nullAge = do
      string "null"
      return $ \p -> p { age = Nothing }

    patProcs :: Parser (Patient -> Patient)
    patProcs = do
      string "procs="
      mod <- string "partial" <|> string "complete"
      let f = if mod=="partial" then Partial else Complete
      cs <- codes
      return $ \p -> p { procs = (f cs) }
    
    patDiags :: Parser (Patient -> Patient)
    patDiags = do
      string "diags="
      mod <- string "partial" <|> string "complete"
      let f = if mod=="partial" then Partial else Complete
      cs <- codes
      return $ \p -> p { diags = (f cs) }

qualityMeasures :: Parser [(String, (Numerator, Cond))]
qualityMeasures = spaces *> many qualityMeasure <* eof
  where
    qualityMeasure :: Parser (String, (Numerator, Cond))
    qualityMeasure = do
      key <- many1 digit
      spaces
      val <- between (char '{') (char '}') $ do
                   spaces
                   numer <- numerator
                   spaces
                   denom <- cond
                   spaces
                   return (numer, denom)
      spaces
      return (key, val)

    numerator :: Parser Numerator
    numerator = between (char '[') (char ']') (numOptn `sepBy` (char ',' >> spaces))
      where
        numOptn :: Parser NumeratorOption
        numOptn = between (char '(') (char ')') ((,) <$> (spaces *> many1 (noneOf "@") <* spaces <* char '@' <* spaces) <*> (code <* spaces))


cond :: Parser Cond
cond = try propC <|> andC <|> orC <|> notC
  where
    conds :: Parser [Cond]
    conds = between (char '[') (char ']') (cond `sepBy` (char ',' >> spaces))
      
    andC :: Parser Cond
    andC = field "And" And conds

    orC :: Parser Cond
    orC = field "Or" Or conds

    notC :: Parser Cond
    notC = field "Not" Not cond

    propC :: Parser Cond
    propC = Prop <$> (ageGEQ <|> cpts <|> icd10s <|> qstn)

    ageGEQ :: Parser Prop
    ageGEQ = AgeGEQ <$> (string "AgeGEQ" >> spaces >> nat)

    cpts :: Parser Prop
    cpts = field2 "CPT" CPT quotes codes
    
    icd10s :: Parser Prop
    icd10s = field2 "ICD10" ICD10 quotes codes
           
    qstn :: Parser Prop
    qstn = field "Q" Q quotes
         

