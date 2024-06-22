-- sum type
data PizzaType = Italian | Thin | Polish -- deriving Show -- like in rust - declare methods to derive (show == tostring)

-- type name | constructor name
data Point = NewPoint Int Int deriving (Show, Eq, Ord)

type Name = String --type alias
data Pet = Cat | Dog Name deriving Show
-- x = Cat -- constructor names Cat and Dog
-- x = Dog "foo"


-- generics aka polymorphic parameters
data Pair a b = Pair a b deriving (Show, Eq, Ord)

main :: IO ()
main = do
    putStrLn $ "hello, I like to " ++ pizzaToString Italian
    putStrLn $ "first of " ++ show someTup ++ " == " ++ show firstEl
    let (_,second,_) = someTup in putStrLn $ "and second is " ++ show second
    where (firstEl,_,_) = someTup

-- pattern match
pizzaToString :: PizzaType -> String
pizzaToString Italian = "munch munch"
pizzaToString Thin = "just regular"
pizzaToString Polish = "this is thicc"

someTup :: (Int, Int, Int)
someTup = (1,2,3)

-- someList !! 2 to get 3
someList :: [Int]
someList = [1,2,3]

-- length to get len
-- concat concats
-- concat without other args flatens array
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The simons"]
allAwesome = [awesome, also]

-- typeclass for equality. a is generic here
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == reverse a

-- generic fun, nothing more can be done with this signature
identity :: a -> a
identity x = x