module LinkedList
    ( printLinkedListResult
    ) where

data List a = Nil | Cons a (List a)
            deriving (Show, Eq)

len :: List a -> Int
len Nil          = 0
len (Cons _ xs)  = 1 + len xs

insert :: a -> List a -> List a
insert x xs = Cons x xs

initList :: (Num a) => List a
initList = insert 40
         $ insert 30
         $ insert 20
         $ insert 10 Nil

initLoopList :: (Num a) => List a
initLoopList =
        let list    = insert 0 list1
            list1   = insert 1 list2
            list2   = insert 2 list3
            list3   = insert 3 list4
            list4   = insert 4 list5
            list5   = insert 5 list6
            list6   = insert 6 list7
            list7   = insert 7 list8
            list8   = insert 8 list9
            list9   = insert 9 list10
            list10  = insert 10 list4
        in list

listNext :: List a -> List a
listNext Nil          = Nil
listNext (Cons _ xs)  = xs

loopDetecting' :: (Eq a) => List a -> List a -> Bool
loopDetecting' current next
      | current == Nil || next1 == Nil || next2 == Nil  = False
      | current == next1 || current == next2            = True
      | otherwise = loopDetecting' (listNext current) next2
      where
          next1 = listNext next
          next2 | next1 == Nil  = Nil
                | otherwise     = listNext next1

loopDetecting :: (Eq a) =>  List a -> Bool
loopDetecting Nil         = False
loopDetecting (Cons _ xs) = loopDetecting' xs xs

printLinkedListResult :: IO ()
printLinkedListResult =
        let
            list1 = initList
            list2 = initList
            isEq = list1 == list2
            currentList = "Current list: " ++ show list2 ++ "\n"
            listEqual = "Equal list: " ++ show isEq ++ "\n"
            listLength = "List length: " ++ (show $ len list1) ++ "\n"
            loopList
                | True == loopDetecting initLoopList = "Loop list: linked list loopd detected\n"
                | otherwise =  "Loop list: " ++ show initLoopList ++ "\n"
        in putStrLn $ currentList ++ listLength ++ listEqual ++ loopList