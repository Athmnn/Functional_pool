--
-- DoOp.hs for undefined in /home/auguste/delivery/tek2/Func_pool/B-FUN-300-STG-3-1-funPoolDay2-auguste.thomann
--
-- Made by 
-- Login   <>
--
-- Started on  Wed Jan 27 09:05:38 2021 
-- Last update Thu Jan 27 17:32:30 2021 
--

import Data.Char
import Prelude

myIsOnlyDigit :: [Char] -> Bool 
myIsOnlyDigit x = all isDigit x

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

myNth :: [a] -> Int -> a
myNth [] _ = error "List error"
myNth (x:xs) 0 = x
myNth (x:xs) y 
    | y < 0 = error "Negative index"
    | otherwise = myNth xs (y - 1)

myIsNeg :: Int -> Bool
myIsNeg x 
    | x < 0 = True
    | x >= 0 = False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
    | a == x = True
    | otherwise = myElem a xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing 
safeDiv x y = Just (x `div` y)

safeNth :: [a] -> Int -> Maybe a 
safeNth x i | i < 0 = Nothing
          | i >= length x = Nothing
safeNth (x:xs) i = if i == 0 then Just x else safeNth xs (i - 1)


safeSucc :: Maybe Int -> Maybe Int
safeSucc  Nothing = Nothing
safeSucc x  = fmap(+1)x

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup y (x:xs)
    | y == myFst x = Just (mySnd x)
    |otherwise = myLookup y xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _ = Nothing
maybeDo _ _ Nothing = Nothing
maybeDo x (Just y) (Just z) = Just (x y z)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing 
readInt x = case myIsOnlyDigit x of
    False -> Nothing 
    True -> Just (read x :: Int)