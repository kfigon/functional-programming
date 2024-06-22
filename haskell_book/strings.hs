main :: IO()
main = do -- do is required to sequence actions/effects (IO)
    putStrLn hello
    putStr "1, 2, 3"
    putStrLn emptyLine
    putStrLn ("goodbye " ++ title ++ "\n")
    let results = all (== True) [
            'f' == head "foo",
            "foo" == (tail "afoo"),
            "o" == (drop 2 "foo"),
            "foobar" == (take 6 "foobarzzzasdf"),
            cur == "Curry is awesome"++"!",
            'y' == cur !! 4,
            "awesome!" == drop 9 cur 
            
            ]
    print results
    where
        emptyLine = ""
        title = "sir"

hello :: String -- alias for [Char]
hello = "count with me"

cur = "Curry is awesome!"


extractStr :: String -> Int -> Int -> String
extractStr x start len = take len $ drop start x
        -- take len (drop start x) -- dollar to chain them and handle predescence

rvrs = let
        aw = take awesomeLen (drop awesomeStart cur)
        is = take 2 $ drop isStart cur
        curry = take curryLen cur
        in aw ++ " " ++ is ++ " " ++ curry
        where 
                awesomeLen = 7
                awesomeStart = 9
                isStart = 6
                curryLen = 5
