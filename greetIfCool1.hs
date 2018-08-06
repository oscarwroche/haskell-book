module GreetIfCool1 where

greetIfCool1 :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "ey whats good"
  else
    putStrLn "fuck outta here bish"
  where cool = coolness == "frosty"

greetIfCool2 :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "ey whats good"
  else
    putStrLn "fuck outta here bish"
  where cool v = v == "frosty"
