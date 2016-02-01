module QualityMeasures where
import Control.Monad.Writer

-- datatypes ------------------------------------------------

data Patient = Patient { age :: Maybe Int, 
                         procs :: CodeList,
                         diags :: CodeList }
  deriving (Show,Eq)

data CodeList = Partial [Code] | Complete [Code]
  deriving (Show,Eq)

type Code = String

data Prop = AgeGEQ Int 
          | CPT String [Code]
          | ICD10 String [Code]
          | Q String
  deriving (Read,Show,Eq)

data Conditional p = Prop p 
                   | Not (Conditional p)
                   | And [Conditional p]
                   | Or [Conditional p] 
  deriving (Read,Show,Eq)
  
instance Functor Conditional where
  fmap f (Prop p) = Prop (f p)
  fmap f (Not c) = Not (fmap f c)
  fmap f (And cs) = And (fmap (fmap f) cs)
  fmap f (Or cs) = Or (fmap (fmap f) cs)

instance Foldable Conditional where
  foldMap f (Prop p) = f p
  foldMap f (Not c) = foldMap f c
  foldMap f (And cs) = foldMap (foldMap f) cs
  foldMap f (Or cs) = foldMap (foldMap f) cs

instance Traversable Conditional where
  traverse f (Prop p) = Prop <$> f p
  traverse f (Not c) = Not <$> traverse f c
  traverse f (And cs) = And <$> traverse (traverse f) cs
  traverse f (Or cs) = Or <$> traverse (traverse f) cs

type Cond = Conditional Prop

type Test = (Bool, Prop)

-- methods ---------------------------------------------------

--Uses patient info to evaluate a proposition.
--Returns Nothing if the proposition cannot be evaluated.
evalProp :: Prop -> Patient -> Maybe Bool
evalProp (AgeGEQ n) pat = fmap (>=n) (age pat)
evalProp (CPT _ xs) pat = case procs pat of
                            Complete ps -> Just $ any (`elem` ps) xs
                            Partial qs -> if any (`elem` qs) xs
                                          then Just True
                                          else Nothing
evalProp (ICD10 _ xs) pat = case diags pat of
                            Complete ps -> Just $ any (`elem` ps) xs
                            Partial qs -> if any (`elem` qs) xs
                                          then Just True
                                          else Nothing
evalProp (Q _) _ = Nothing


--Uses patient info to evaluate a boolean expression.
--If there is enough information to determine the 
--expression, then a boolean is returned, along with a
--list of results for each proposition. Otherwise,
--a new boolean expression is returned containing
--only propositions still undetermined.
evalCond :: Cond -> Patient -> (Either Cond Bool, [Test])
evalCond cond pat = runWriter reduced
  where
    reduced :: Writer [Test] (Either Cond Bool)
    reduced = toCond . fmap prune . fromCond $ cond
    
    fromCond = traverse evalProp'
    toCond mc = do c <- mc
                   case c of
                     (Prop (Right b)) -> return $ Right b
                     c -> return $ Left (fmap unpack c)
    unpack (Left prop) = prop
    unpack (Right b) = error "Prune failed to remove all determined branches."

    evalProp' :: Prop -> Writer [Test] (Either Prop Bool)
    evalProp' pr = case evalProp pr pat of
      Nothing -> return $ Left pr
      Just b -> do tell [(b, pr)]
                   return $ Right b

    prune :: Conditional (Either p Bool) -> Conditional (Either p Bool)
    prune (Prop x) = Prop x
    prune (Not (Prop (Right b))) = Prop (Right (not b))
    prune (Not (Not x)) = prune x
    prune (Not x) = Not (prune x)
    prune (And cs) = let x = filter (not.boolProp True) . map prune $ cs
                      in if any (boolProp False) x
                           then Prop (Right False)
                           else if null x
                             then Prop (Right True)
                             else And x
    prune (Or cs) = let x = filter (not.boolProp False) . map prune $ cs
                     in if any (boolProp True) x
                          then Prop (Right True)
                          else if null x
                            then Prop (Right False)
                            else Or x

    boolProp :: Bool -> Conditional (Either p Bool) -> Bool
    boolProp b (Prop (Right b')) = b == b'
    boolProp b _ = False




