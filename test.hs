sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3 + 2

multiPi x = 3.14 * x ^ 2
