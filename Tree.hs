module Tree where

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

data InternalTree a = ILeaf
                    | IBranch a (InternalTree a) (InternalTree a)
                    deriving Show

data FancyTree a b = FLeaf a
                   | FBranch b (FancyTree a b) (FancyTree a b)
                   deriving Show

mapTree                 :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x)       = Leaf (f x)
mapTree f (Branch t1 t2) = Branch (mapTree f t1)
                                  (mapTree f t2)

foldLeaf                     :: (a -> b -> a) -> a -> Tree b -> a
foldLeaf f acc (Leaf x)       = f acc x
foldLeaf f acc (Branch t1 t2) = foldLeaf f (foldLeaf f acc t1) t2

fringe :: Tree a -> [a]
fringe  = foldLeaf (\ acc x -> acc ++ [x]) []

treeSize :: Tree a -> Integer
treeSize  = foldLeaf (\ acc _ -> acc + 1) 0

foldBranch                         :: (a -> b -> a) -> (a -> a -> a) -> a ->
                                            Tree b -> a
foldBranch fl _ acc (Leaf x)        = fl acc x
foldBranch fl fb acc (Branch t1 t2) = fb (foldBranch fl fb acc t1)
                                         (foldBranch fl fb acc t2)

treeHeight :: Tree a -> Integer
treeHeight  = foldBranch (\ _ _ -> 0) (\ b1 b2 -> 1 + max b1 b2) 0

data Expr = C Float | Expr :+ Expr | Expr :- Expr | Expr :* Expr
          | Expr :/ Expr | V String | Let String Expr Expr

evaluate                          :: Expr -> Float
evaluate (C x)                     = x
evaluate (V _)                     = error "Cannot evaluate unbound variable"
evaluate (e1 :+ e2)                = evaluate e1 + evaluate e2
evaluate (e1 :- e2)                = evaluate e1 - evaluate e2
evaluate (e1 :* e2)                = evaluate e1 * evaluate e2
evaluate (e1 :/ e2)                = evaluate e1 / evaluate e2
evaluate (Let variable value expr) =
    evaluate (rewrite variable (evaluate value) expr)
    where rewrite :: String -> Float -> Expr -> Expr
          rewrite _ _ c@(C _) = c
          rewrite s v d@(V name)
            | s == name = C v
            | otherwise = d
          rewrite s v (e1 :+ e2) = rewrite s v e1 :+ rewrite s v e2
          rewrite s v (e1 :- e2) = rewrite s v e1 :- rewrite s v e2
          rewrite s v (e1 :* e2) = rewrite s v e1 :* rewrite s v e2
          rewrite s v (e1 :/ e2) = rewrite s v e1 :/ rewrite s v e2

takeTree                    :: Int -> InternalTree a -> InternalTree a
takeTree 0 _                 = ILeaf
takeTree _ ILeaf             = ILeaf
takeTree n (IBranch x t1 t2) = IBranch x
                                       (takeTree (n - 1) t1)
                                       (takeTree (n - 1) t2)

takeTreeWhile                    :: (a -> Bool) -> InternalTree a ->
                                        InternalTree a
takeTreeWhile _ ILeaf             = ILeaf
takeTreeWhile f (IBranch x t1 t2)
    | f x       = IBranch x (takeTreeWhile f t1) (takeTreeWhile f t2)
    | otherwise = ILeaf

foldrITree                        :: (a -> b -> b) -> b -> InternalTree a -> b
foldrITree _ acc ILeaf             = acc
foldrITree f acc (IBranch x t1 t2) = foldrITree f
                                                (foldrITree f (f x acc) t2)
                                                t1

repeatITree :: a -> InternalTree a
repeatITree x = IBranch x (repeatITree x) (repeatITree x)

zipWithITree                                      :: (a -> b -> c) ->
                                                         InternalTree a ->
                                                         InternalTree b ->
                                                         InternalTree c
zipWithITree _ ILeaf _                             = ILeaf
zipWithITree _ _ ILeaf                             = ILeaf
zipWithITree f (IBranch x t1 t2) (IBranch y t3 t4) =
    IBranch (f x y) (zipWithITree f t1 t3) (zipWithITree f t2 t4)

zipITree :: InternalTree a -> InternalTree b -> InternalTree (a, b)
zipITree  = zipWithITree (\ x y -> (x, y))
