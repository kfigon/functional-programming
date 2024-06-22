import Data.Char (toLower,toUpper,isUpper, isSpace)

-- In every year that is evenly divisible by 4.
-- Unless the year is evenly divisible by 100, in which case it's only a leap year if the year is also evenly divisible by 400.
-- Some examples:

-- 1997 was not a leap year as it's not divisible by 4.
-- 1900 was not a leap year as it's not divisible by 400.
-- 2000 was a leap year!

isLeap :: Int -> Bool
isLeap y
    | y `mod` 400 == 0 = True
    | y `mod` 100 == 0 = False
    | y `mod` 4 == 0 = True
    | otherwise = False

reverseStr :: String -> String
reverseStr "" = ""
reverseStr x = let firstChar = head x in
    reverseStr $ tail x ++ [firstChar]

-- different syntax for head-tail:
-- reverseStr (x:xs) = reverseStr xs ++ [x]

-- If the dart lands outside the target, player earns no points (0 points).
-- If the dart lands in the outer circle of the target, player earns 1 point.
-- If the dart lands in the middle circle of the target, player earns 5 points.
-- If the dart lands in the inner circle of the target, player earns 10 points.

-- The outer circle has a radius of 10 units (this is equivalent to the total radius for 
-- the entire target), the middle circle a radius of 5 units, and the inner circle a radius of 1.
--  Of course, they are all centered at the same point — that is, the circles are concentric 
--  defined by the coordinates (0, 0).

-- Write a function that given a point in the target (defined by its Cartesian coordinates x and y, 
-- where x and y are real), 
-- returns the correct amount earned by a dart landing at that point.

data TargetArea = Inner | Middle | Outer | Outside

targetToPoints :: TargetArea -> Int
targetToPoints Inner = 10
targetToPoints Middle = 5
targetToPoints Outer = 1
targetToPoints Outside = 0

coordToTargetArea :: Float -> Float -> TargetArea
coordToTargetArea x y
    | distanceToCenter < 1 = Inner
    | distanceToCenter < 5 = Middle
    | distanceToCenter < 10 = Outer
    | otherwise = Outside
    where distanceToCenter = sqrt (x*x + y*y)

darts :: Float -> Float -> Int
darts x y = targetToPoints $ coordToTargetArea x y

-- pangram is a word that uses all the letters of the english alphabet (26), case insensitive
-- The quick brown fox jumps over the lazy dog
pangram :: String -> Bool
pangram text = all cmp allCharacters
    where
        allCharacters = ['a'..'z']
        loweredText = map toLower text
        cmp alphabetChar = elem alphabetChar loweredText
        -- infix notation can simplify, as it'll be auto curried. I curried manually here:
        -- all (`elem` loweredText) allCharacters


-- bob just responds to you by some phrases
-- "Sure." This is his response if you ask him a question, such as "How are you?" The convention used for questions is that it ends with a question mark.
-- "Whoa, chill out!" This is his answer if you YELL AT HIM. The convention used for yelling is ALL CAPITAL LETTERS.
-- "Calm down, I know what I'm doing!" This is what he says if you yell a question at him.
-- "Fine. Be that way!" This is how he responds to silence. The convention used for silence is nothing, or various combinations of whitespace characters.
-- "Whatever." This is what he answers to anything else.
bob :: String -> String
bob phrase
    | phrase == "" || isSilence = "Fine. Be that way!"
    | isQuestion && allUpper = "Calm down, I know what I'm doing!"
    | isQuestion = "Sure."
    | allUpper = "Whoa, chill out!"
    | otherwise = "Whatever."
    where
        allUpper = phrase == map toUpper phrase
        isQuestion = last phrase == '?'
        isSilence = all isSpace phrase


-- The Collatz Conjecture or 3x+1 problem can be summarized as follows:
-- Take any positive integer n. If n is even, divide n by 2 to get n / 2. 
-- If n is odd, multiply n by 3 and add 1 to get 3n + 1. Repeat the process indefinitely. 
--The conjecture states that no matter which number you start with,you will always reach 1 eventually.
-- Given a number n, return the number of steps required to reach 1.
-- n = 12 -> 9 steps
collatz :: Int -> Int
collatz x
    | even x = x `div` 2
    | otherwise = 3*x + 1

newtype NumberOfSteps = NumberOfSteps Int deriving Show
conjecture :: Int -> NumberOfSteps
conjecture x = calcStep (NumberOfSteps 0) x
    where
        calcStep :: NumberOfSteps -> Int -> NumberOfSteps
        calcStep (NumberOfSteps step) num
            | num == 1 = NumberOfSteps step
            | otherwise = calcStep (NumberOfSteps $ step+1) (collatz num)


-- The four nucleotides found in DNA are adenine (A), cytosine (C), guanine (G) and thymine (T).
-- The four nucleotides found in RNA are adenine (A), cytosine (C), guanine (G) and uracil (U).
-- Given a DNA strand, its transcribed RNA strand is formed by replacing each 
-- nucleotide with its complement:
-- G -> C
-- C -> G
-- T -> A
-- A -> U
data DNA = DnaAdenine | DnaCytosine | DnaGuanine | DnaThymine deriving Show
data RNA = RnaAdenine | RnaCytosine | RnaGuanine | RnaUracil deriving Show

dnaToRna :: DNA -> RNA
dnaToRna DnaGuanine = RnaCytosine
dnaToRna DnaCytosine = RnaGuanine
dnaToRna DnaThymine = RnaAdenine
dnaToRna DnaAdenine = RnaUracil


-- putStrLn accepts strings, `.` combines functions
myOwnPrint :: Show a => a -> IO ()
myOwnPrint x = (putStrLn . show) x