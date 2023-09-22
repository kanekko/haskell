
myButLast :: [a] -> a

myButLast [x,_] = x
myButLast (_:xs) = myButLast xs


myButLast2 :: [a] -> a

myButLast2 = head . tail . reverse












