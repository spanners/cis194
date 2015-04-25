module Party where

import Employee
import Data.Monoid

glCons :: Employee ->GuestList -> GuestList
glCons e (GL l n) = (GL  (e : l) (n + empFun e))

instance Monoid GuestList where
  mempty = (GL [] 0)
  mappend (GL l1 n1) (GL l2 n2) = (GL (l1 ++ l2) (n1 + n2))

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max
