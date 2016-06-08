module Ex where
doubleMe x = x*2
doubleSmallNumber x = if x >100
                      then x
                      else x*2
data List a = Empty 
            |Node a (List a)
            deriving (Show)

exLength :: List a -> Int
exLength Empty = 0
exLength (Node x xs) = 1 + (exLength xs)

exFirst :: List a -> a
exFirst Empty = error "empty list"
exFirst (Node x _) = x

exTail :: List a -> List a
exTail Empty = Empty
exTail (Node x xs) = xs

exLast :: List a -> a
exLast Empty = error  "Empty list"
exLast (Node x Empty) =  x
exLast (Node x xs) = exLast xs

isThisThree :: Int -> Bool
isThisThree 3 = True
isThisThree _ = False
