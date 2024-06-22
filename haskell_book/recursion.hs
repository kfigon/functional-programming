main :: IO ()
main = do
    putStrLn "hello"

factorial x
    | x <= 1 = 1
    | otherwise = x* factorial(x-1)