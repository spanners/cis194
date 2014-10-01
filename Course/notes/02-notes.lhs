Hi, this is a literate haskell file!

> data Thing = Shoe
>            | Ship
>            | SealingWax
>            | Cabbage
>            | King
>     deriving Show

> data Person = Person String Int Thing
>     deriving Show

> baz :: Person -> String
> baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

> main :: IO ()
> main = putStrLn $ baz (Person "Brent" 31 SealingWax)
