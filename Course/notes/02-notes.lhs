What?
=====

Hi, this is a literate haskell file!

> data Thing = Shoe
>            | Ship
>            | SealingWax
>            | Cabbage
>            | King
>     deriving Show

> data Person = Person String Int Thing
>     deriving Show

We can use the `@` sign in patterns to create a definition for the whole value being matched against.

> baz :: Person -> String
> baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

Result:

````
*ghci> baz brent
"The name field of (Person "Brent" 31 SealingWax) is Brent"
````

> main :: IO ()
> main = putStrLn $ baz (Person "Brent" 31 SealingWax)


