data DayOfWeek = 
    Monday |
    Tuesday |
    Wednesday | 
    Thursday |
    Friday |
    Saturday |
    Sunday 
    -- deriving Show

instance Eq DayOfWeek where
    (==) Monday Monday = True
    (==) Tuesday Tuesday = True
    (==) Wednesday Wednesday = True
    (==) Thursday Thursday = True
    (==) Friday Friday = True
    (==) Saturday Saturday = True
    (==) Sunday Sunday = True
    (==) _ _ = False

-- instance Show DayOfWeek where
--     Monday = "Monday"
--     Tuesday = "Tuesday"
--     Wednesday = "Wednesday"
--     Thursday = "Thursday"
--     Friday = "Friday"
--     Saturday = "Saturday"
--     Sunday = "Sunday"


-- constraint only "a"s that implement Eq
data Foo a = Foo a
instance Eq a => Eq (Foo a) where
    (==) (Foo x) (Foo y) = x == y

-- constraint on function - allo only Num params
pluz :: Num a => a -> a -> a
pluz a b = a+b

data Mood = Bleh
instance Show Mood where
    show _ = "booring"

-- my own typeclass, require a function foobar that returns bool
class MyOwnTypeclass a where
    foobar :: a -> Bool


data Temperature = C Float | F Float
-- f = F 1.2
instance Eq Temperature where
    (==) (C c) (C c') = c == c'
    (==) (F f) (F f') = f == f'
    (==) (F f) (C c) = (1.8*c + 32)  == f
    (==) (C c) (F f) = (1.8*c + 32)  == f

instance Show Temperature where
    show (C c) = show c ++ "C"
    show (F f) = show f ++ "F"