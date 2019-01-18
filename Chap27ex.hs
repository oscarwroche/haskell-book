
-- x = undefined
y = "blah"
main = let !x = undefined in 
  do
    print (snd (x, y))