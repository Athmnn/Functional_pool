--
-- My.hs for undefined in /home/auguste/delivery/tek2/Func_pool/B-FUN-300-STG-3-1-funPoolDay1-auguste.thomann
--
-- Made by 
-- Login   <>
--
-- Started on  Tue Jan 26 09:16:46 2021 
-- Last update Wed Jan 26 22:19:50 2021 
--

mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x 
    | x < 0 = True
    | x >= 0 = False

myAbs :: Int -> Int
myAbs x
    | x > 0 = x
    | x < 0 = x * (-1)

myMin :: Int -> Int -> Int
myMin x y = case x > y of
    True  -> y
    False -> x

myMax :: Int -> Int -> Int
myMax x y = case x > y of
    True -> x
    False -> y

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple  a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead[] = error "Empty list"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail[] = error "Empty list"
myTail (x:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength(x:xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth [] _ = error "List error"
myNth (x:xs) 0 = x
myNth (x:xs) y 
    | y < 0 = error "Negative index"
    | otherwise = myNth xs (y - 1)

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = (x: myTake (n - 1) xs)

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) 
    | n > myLength xs || n < 0 = error "Index error"
    | otherwise = myDrop (n - 1) xs

myAppend :: [a] -> [a] -> [a]
myAppend xs [] = xs
myAppend [] ys = ys
myAppend (x:xs) y = x : myAppend xs y

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit x = myReverse(myTail(myReverse x)) 

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast x = myNth x (myLength x - 1)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (myTuple x y) : myZip xs ys


myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([],[])
myUnzip ((a,b):xs) = (a:(myFst (myUnzip xs)), b:(mySnd (myUnzip xs)))

myMap :: (a -> b) -> [a] -> [b]
myMap x [] = []
myMap x (y:ys) = x y : myMap x ys

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter y (x:xs) = case y x of
    True ->(x:myFilter y xs)
    False -> myFilter y xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ n [] = n
myFoldl y n (x:xs) = myFoldl y n (myReverse xs)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr y n xs = myFoldr y n (myReverse xs)

