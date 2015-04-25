module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

glCons :: Employee ->GuestList -> GuestList
glCons e (GL l n) = (GL  (e : l) (n + empFun e))

instance Monoid GuestList where
  mempty = (GL [] 0)
  mappend (GL l1 n1) (GL l2 n2) = (GL (l1 ++ l2) (n1 + n2))

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f l (Node a ts) = f a l'
  where l' = map (treeFold f l) ts

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (bestWith, bestWithout)
  where bestWith = glCons b $ mconcat $ map snd gls
        bestWithout = mconcat $ map (uncurry moreFun) gls

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel []

formatGuestList :: GuestList -> [String]
formatGuestList (GL emps fun) = ("Total fun: " ++ show fun) : sort (map empName emps)

main :: IO ()
main = do
  company <- readFile "company.txt"
  let party = maxFun (read company :: Tree Employee)
  mapM_ putStrLn (formatGuestList party)