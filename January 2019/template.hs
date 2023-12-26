module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp x xys = head [v | (i, v) <- xys, i == x]

-- 3 marks
vars :: Formula -> [Id]
vars (Var x)     = [x]
vars (Not f)     = vars f
vars (And f1 f2) = (sort . nub) (vars f1 ++ vars f2)
vars (Or f1 f2)  = (sort . nub) (vars f1 ++ vars f2)
-- 1 mark
idMap :: Formula -> IdMap
idMap f = zip (vars f) [1..]
--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Or a b))  = toNNF (And (Not (toNNF a)) (Not (toNNF b)))
toNNF (Not (And a b)) = toNNF (Or  (Not (toNNF a)) (Not (toNNF b)))
toNNF (Not (Not a))   = toNNF a
toNNF (Not f)         = Not (toNNF f)
toNNF (And f1 f2)     = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2)      = Or (toNNF f1) (toNNF f2)
toNNF f               = f

-- 3 marks
toCNF :: Formula -> CNF
toCNF = toCNF' . toNNF
 where
    toCNF' :: NNF -> CNF
    toCNF' (Or a f@(And b c)) = distribute a f
    toCNF' (Or f@(And a b) c) = distribute f c
    toCNF' (Or f1 f2)         = Or (toCNF' f1) (toCNF' f2)
    toCNF' (And f1 f2)        = And (toCNF' f1) (toCNF' f2) 
    toCNF' f = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f = flatten' f
 where
    id = idMap f
    flatten' :: CNF -> CNFRep
    flatten' (Var x)       = [[lookUp x id]]
    flatten' (Not (Var x)) = [[- lookUp x id]]
    flatten' (Or f1 f2)    = [((concat . flatten') f1) ++ ((concat . flatten') f2)]
    flatten' (And f1 f2)   = flatten' f1 ++ flatten' f2
--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits repp
 | null units = (repp, [])
 | otherwise  = (repp', u : us)   
 where 
    units = (filter ((== 1) . length) repp)
    u = (head . head) units
    newRepp = map (filter ( /= -u)) (filter (notElem u) repp) -- clause deletion (filter clauses which dont have u) + literal deletion (filter literals inside clauses which not are negative of u)
    (repp', us) = propUnits newRepp
-- 4 marks
dp :: CNFRep -> [[Int]]
dp repp
--  | (not . null) (filter null repp) = []
 | [] `elem` repp                  = []
 | null repp'                      = [initUnits]
 | otherwise                       = [initUnits ++ l | l <- answer]
 where (repp', initUnits) = propUnits repp
       u                  = (head . head) repp'
       answer             = dp ([[u]] ++ repp') ++ dp ([[-u]] ++ repp')
--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f = (map fun allsols)
 where sols = (dp . flatten . toCNF) f
       idmap = idMap f
       va    = vars f
       nums = map (snd) idmap -- the numbers assigned to vars
       solsPlusNotThere = map (\sol -> (sol, filter (`notElem` (map abs sol)) nums)) sols  -- the sols list with list of vars which are not there
       allsols = concatMap getAllSols solsPlusNotThere
       getAllSols :: ([Int],[Int]) -> [[Int]]
       getAllSols (haha, [])       = [haha]
       getAllSols (haha, (x : xs)) = [[x] ++ l | l <- getAllSols (haha, xs)] ++ [[-x] ++ l | l <- getAllSols (haha,xs)]
       fun :: [Int] -> [(Id, Bool)]
       fun xs = zip va (map (`elem` xs) nums)