module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x = (length . filter (== x))

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (nodes, edges) = [(u, length ((filter ((== u) . fst) edges) ++ (filter ((== u) . snd) edges))) | u <- nodes]

neighbours :: Eq a => a -> Graph a -> [a]

neighbours u (nodes, edges)
 | u `notElem` nodes = error "node not in the graph"
 | otherwise         = [v | v <- nodes, ((u, v) `elem` edges || (v, u) `elem` edges)]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode u (nodes, edges)
 | u `notElem` nodes = error "node not in the graph"
 | otherwise         = (filter (/= u) nodes, filter (\(x, y) -> x /= u && y /= u) edges)
------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _   ([], _) = []
colourGraph lim g@(nodes, edges) = (n, c) : cMapping
 where degree    = degrees g
       n         = (fst . head) (filter ((== minimum (map snd degree)) . snd) degree)
       cMapping  = colourGraph lim (removeNode n g)
       c         = extractCol (nub (map (`lookUp` cMapping) (neighbours n g)))
       
       safeHead :: [Int] -> Int
       safeHead []        = 0
       safeHead ( x : xs) = x

       extractCol :: [Int] -> Int
       extractCol cols = safeHead ([1..lim] \\ cols)  
    --    extractCol cols = safeHead [i | i <- [1..lim], i `notElem` cols]
------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap coloring = ("return", "return") : mapping
 where mapping = map (\(id, c) -> if c == 0 
                                  then (id, id)
                                  else (id, "R" ++ (show c))) coloring

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments args mapp = [Assign (lookUp arg mapp) (Var arg) | arg <- args]

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var x) napp          = Var (lookUp x napp)
renameExp (Apply o e1 e2) napp  = Apply o (renameExp e1 napp) (renameExp e2 napp) 
renameExp e _                   = e
renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap.
renameBlock [] kapp                  = [] 
renameBlock (Assign x e : bs) kapp
 | (Var lhs) == rhs = renameBlock bs kapp
 | otherwise        = (Assign lhs rhs) : renameBlock bs kapp
 where lhs = lookUp x kapp 
       rhs = renameExp e kapp
renameBlock (If p q r : bs) kapp      = (If (renameExp p kapp) (renameBlock q kapp) (renameBlock r kapp)) : renameBlock bs kapp
renameBlock (While p block : bs) kapp = (While (renameExp p kapp) (renameBlock block kapp)) : renameBlock bs kapp

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG vars = (nodes, edges) 
 where nodes  = (nub . concat) vars 
       edges  = [(x, y) | i <- [0..length nodes - 1], 
                          j <- [i+1..length nodes - 1],
                          let x = (nodes !! i),
                          let y = (nodes !! j),
                          any (\l -> x `elem` l && y `elem` l) vars ]
                        
-----------------------------------------------------
--
-- Part V
--
-- liveVars :: CFG -> [[Id]]
-- liveVars cfgs 
--  where
    
    
--     liveVars' :: CFG -> [[Id]]
--     liveVars' cfgs = map fun cfgs
    
    
--     fun :: ((Id, [Id]), [Int]) -> [Id]
--     fun (("_", def), succ)
--      | null l    = []
--      | otherwise = live
--      where l = map (\s -> fun (cfgs !! s)) succ 
--            live = (nub (foldr1 union l)) \\ def
--     fun ((use, def), succ)
--      | null l    = [use]
--      | otherwise = union [use] live
--      where l = map (\s -> fun (cfgs !! s)) succ 
--            live = (nub (foldr1 union l)) \\ def

extractVars :: Exp -> [Id]
extractVars (Var x) = [x]
extractVars (Apply o e1 e2) = union ((nub . extractVars) e1) ((nub . extractVars) e2)
extractVars _ = []

buildCFG :: Function -> CFG
buildCFG (name, args, (Assign x e):bs) i = ((x, extractVars e), [i + 1]) : (buildCFG (name, args, bs) (i + 1))
buildCFG (name, args, (If p q r):bs) i   = (("_", extractVars p), [i + 1]) 
