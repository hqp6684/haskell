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
