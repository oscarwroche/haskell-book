module FunctionWithWhereAndLet where

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

printInc2' n =
  (\plusTwo -> print plusTwo) (n + 2)