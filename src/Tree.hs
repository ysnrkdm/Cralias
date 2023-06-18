module Tree where

data Tree n = Node {node :: n, childNodes :: [Tree n]} deriving (Eq)

instance (Show n) => Show (Tree n) where
  show = showAtLvl 0
    where
      showAtLvl lvl (Node n children) = "" ++ indent lvl ++ show n ++ "\n" ++ concatMap (showAtLvl (lvl + 1)) children
      indent lvl = concat $ concat $ replicate lvl ["|-"]

redtree :: (t -> t1 -> t2) -> (t2 -> t1 -> t1) -> t1 -> Tree t -> t2
redtree f g a Node {node = n, childNodes = c} = f n (redtree' f g a c)
  where
    redtree' f g a (hd : rest) = g (redtree f g a hd) (redtree' f g a rest)
    redtree' f g a [] = a

maptree :: (t -> b) -> Tree t -> Tree b
maptree f = redtree (Node . f) (:) []

reptree :: (t -> [t]) -> t -> Tree t
reptree f a = Node a (map (reptree f) (f a))
