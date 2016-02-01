module Flow where
import QualityMeasures

type Flow = (Cond, [Crumb])
data Crumb = NotCrumb | AndCrumb [Cond] | OrCrumb [Cond]

up :: Flow -> Maybe Flow
up (c, []) = Nothing
up (c, NotCrumb : ts) = Just (Not c, ts)
up (And cs, AndCrumb ls : ts) = Just (And (ls ++ cs), ts)
up (c, AndCrumb
up (c, OrCrumb ls rs : ts) = Just (Or (ls ++ [c] ++ rs), ts)

right :: Flow -> Maybe Flow


nextProp :: Maybe Bool -> Flow -> (Maybe Prop, Flow)
nextProp mb (c, ts) = case c of
  
