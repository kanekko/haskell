

myLast :: [a] -> a

myLast [] = error "Empty List"
myLast [x] = x
myLast (_:xs) = myLast xs


myLast2 :: [a] -> a
myLast2 = head . reverse




















