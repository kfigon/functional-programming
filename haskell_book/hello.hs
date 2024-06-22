-- module Hello where -- not required for simple scripts
-- modules should be names the same as their filenames
-- module Hello(calcIt) where - this will export only calcIt function

-- () - unit type
sayHello :: String -> IO ()
sayHello x = putStrLn ("hello " ++ x)

-- functions aceept 1 argument and return result. multiple arguments are supported by currying
addIt :: Int -> Int -> Int -- type declarations are optional
addIt x y = x + y

-- where is a declaration
-- let introduces an expression - define blocks that build an expression
calcIt :: Int -> Int
calcIt x = tripple oneMore + constant
    where 
        constant = 4
        tripple a = a*3
        oneMore = x+1

-- same, but using let
calcIt2 :: Int -> Int
calcIt2 x =     
    let 
        constant = 4
        tripple a = a*3
        oneMore = x+1
    in tripple oneMore + constant
-- let { constant = 4; tripple a = a*3; oneMore = x+1 } in tripple oneMore + constant -- in one line

-- if-else (no elsif)
compareval :: Int -> String
compareval n = 
    if n > 0
        then "positivie"
    else
        if n < 0 then "negative" else "zero"

-- pattern matching
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- pattern matching in case statement
fib2 :: Int -> Int
fib2 n = case n of
    0 -> 1
    1 -> 1
    other -> fib2(n-1) + fib(n-2)

-- function guards
fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0  = "Fizbuzz"
    | n `mod` 5  == 0 = "buzz"
    | n `mod` 3  == 0 = "Fizz"
    | otherwise = show n