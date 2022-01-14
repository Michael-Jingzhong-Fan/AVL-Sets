module Coursework where

import qualified Data.Set as HS (fromList, toList, null, map, member, size, foldr, union, difference, intersection, powerSet, delete, insert, singleton)
import Test.QuickCheck
import Test.HUnit

data Set a = BLANK | Vertex a (Set a) (Set a) deriving (Show, Ord)

toList :: Set a -> [a]
toList BLANK = []
toList (Vertex a le ri) = toList (le) ++ [a] ++ toList (ri)

fromList :: Ord a => [a] -> Set a
fromList xs = buildAVL . removeDuplicatesList . sortedList $ xs
    
sortedList [] = []
sortedList (x:xs) = (sortedList apples) ++ [x] ++ (sortedList bananas)
   where
      apples = filter (< x) xs
      bananas = filter (>= x) xs

removeDuplicatesList :: (Eq a) => [a] -> [a]
removeDuplicatesList [] = []
removeDuplicatesList (x:xs) = x : removeDuplicatesList (filter (\y -> not(x == y)) xs)
    
buildAVL :: [a] -> Set a
buildAVL [] = BLANK
buildAVL sortedList = Vertex mid (buildAVL bananas) (buildAVL apples)
   where (bananas, carrots) = splitAt ((length sortedList `div` 2) - 1) sortedList
         apples = tail carrots
         mid = head carrots

instance (Ord a) => Eq (Set a) where
   apples == bananas = (toList apples == toList bananas)

empty :: Set a
empty = BLANK

singleton :: a -> Set a
singleton a = Vertex a BLANK BLANK

insert :: (Ord a) => a -> Set a -> Set a
insert x BLANK = singleton x
insert x (Vertex a le ri) 
   | x == a = Vertex x le ri
   | x < a = rotate (Vertex a (insert x le) ri)
   | x > a = rotate (Vertex a le (insert x ri))

rotate :: (Ord a) => Set a -> Set a
rotate BLANK = BLANK
rotate (Vertex a le ri) 
   | not (inBalance (Vertex a le ri)) = rotate . rotateAux $ (Vertex a le ri)
   | not (inBalance le) = rotate $ Vertex a (rotateAux le) ri
   | not (inBalance ri) = rotate $ Vertex a le (rotateAux ri)
   | otherwise = Vertex a le ri
    
rotateAux :: (Ord a) => Set a -> Set a
rotateAux BLANK = BLANK
rotateAux (Vertex a le ri) 
   | cardinality le > cardinality ri = (Vertex x (removeSet x le) (insert a ri)) 
   | cardinality ri > cardinality le = (Vertex y (insert a le) (removeSet y ri))
     where x = rightMinVertex le
           y = leftMinVertex ri

leftMinVertex :: (Set a) -> a
leftMinVertex (Vertex a BLANK _) = a
leftMinVertex (Vertex a le _) = leftMinVertex le
    
removeSet :: (Ord a) => a -> Set a -> Set a
removeSet _ BLANK = BLANK
removeSet x (Vertex a le ri) = rotate . removeItem x $ (Vertex a le ri)
   where removeItem _ BLANK = BLANK
         removeItem x (Vertex a le ri)
            | x == a = remove a (Vertex a le ri)
            | x < a = Vertex a (removeItem x le) ri
            | x > a = Vertex a le (removeItem x ri)
    
remove :: (Ord a) => a -> Set a -> Set a
remove x (Vertex _ BLANK BLANK) = BLANK
remove x (Vertex _ le BLANK) = le
remove x (Vertex _ BLANK ri) = ri
remove x (Vertex _ le ri) = Vertex a le ri'
   where a = leftMinVertex ri
         ri' = (removeSet a ri)
    
inBalance :: (Ord a) => Set a -> Bool
inBalance BLANK = True
inBalance (Vertex a le ri)
   | abs ((cardinality le) - (cardinality ri)) > 1 = False
   | otherwise = True
    
rightMinVertex :: (Set a) -> a
rightMinVertex (Vertex a _ BLANK) = a
rightMinVertex (Vertex a _ ri) = rightMinVertex ri

union :: (Ord a) => Set a -> Set a -> Set a
union BLANK BLANK = BLANK
union BLANK apples = apples
union apples BLANK = apples  
union (Vertex a left right) bananas = setfoldr (insert) bananas (Vertex a left right)

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection BLANK _  = BLANK
intersection _ BLANK = BLANK
intersection (Vertex a le ri) bananas 
   | member a bananas = rotate (Vertex a (intersection le bananas) (intersection ri bananas))
   | otherwise = rotate . union (intersection le bananas) $ (intersection ri bananas)

difference :: (Ord a) => Set a -> Set a -> Set a
difference BLANK _ = BLANK
difference set BLANK = set
difference (Vertex a le ri) bananas 
   | member a bananas = rotate . union (difference le bananas) $ (difference ri bananas)
   | otherwise = rotate (Vertex a (difference le bananas) (difference ri bananas))

member :: (Ord a) => a -> Set a -> Bool
member _ BLANK = False
member x (Vertex a le ri) 
   | x == a = True
   | x < a = member x le
   | x > a = member x ri

cardinality :: Set a -> Int
cardinality BLANK = 0
cardinality (Vertex a le ri) = 1 + cardinality le + cardinality ri

setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap _ BLANK = BLANK
setmap f set = setfoldr (\x acc -> insert (f x) acc) BLANK set

setfoldr :: (a -> b -> b) -> b -> Set a -> b
setfoldr _ acc BLANK = acc
setfoldr f acc (Vertex a le ri) = setfoldr f (f a (setfoldr f acc ri)) le
 
listception :: [a] -> [[a]]
listception xs = []: foldr tog [] xs
    where tog x acc = [x]: map (x:) acc ++ acc

powerSet :: Set a -> Set (Set a)
powerSet BLANK = BLANK
powerSet (Vertex a le ri) = fromList' ([ fromList' xs | xs <- (listception (toList (Vertex a le ri)))])

cartesian :: Set a -> Set b -> Set (a, b)
cartesian BLANK _ = BLANK
cartesian _  BLANK = BLANK
cartesian (Vertex a le ri) (Vertex a1 le1 ri1) = fromList'  [ (x, y) | x <- (toList (Vertex a le ri)), y <- (toList (Vertex a1 le1 ri1))]

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition f BLANK = (BLANK, BLANK)
partition f (Vertex a le ri) = (fromList' (filter f (toList (Vertex a le ri))), fromList' (filter (not.f) (toList (Vertex a le ri))))

fromList' :: [a] -> Set a
fromList' xs = buildAVL xs












--tests


-- Make sure you satisfy this property. If it fails, then all of the functions
    -- on Part 3 will also fail their tests
toFromListProp :: IO ()
toFromListProp =
   quickCheck
      ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

-- you should be able to satisfy this property quite easily
eqProp :: IO ()
eqProp =
   quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)

{-
   PART 3. Your Set should contain the following functions. DO NOT CHANGE THE
   TYPE SIGNATURES.
-}

-- is it the empty set?
isNull :: Set a -> Bool
isNull BLANK = True
isNull _ = False

toFromListProp' :: (Ord a) => [a] -> Bool
toFromListProp' xs = (HS.toList (HS.fromList xs) == (toList (fromList xs)))

isNullProp :: (Ord a) => [a] -> Bool
isNullProp xs = HS.null (HS.fromList xs) == isNull (fromList xs)

singletonProp :: (Ord a) => a -> Bool
singletonProp x = (HS.toList (HS.singleton x)) == (toList (singleton x))

insertProp :: (Ord a) => a -> [a] -> Bool
insertProp x xs = (HS.toList (HS.insert x (HS.fromList xs)) == toList (insert x (fromList xs)))

unionProp :: (Ord a) => [a] -> [a] -> Bool
unionProp xs ys = (HS.toList (HS.union (HS.fromList xs) (HS.fromList ys))) == (toList (union (fromList xs) (fromList ys)))

intersectionProp :: (Ord a) => [a] -> [a] -> Bool
intersectionProp xs ys = (HS.toList (HS.intersection (HS.fromList xs) (HS.fromList ys))) == (toList (intersection (fromList xs) (fromList ys)))

differenceProp :: (Ord a) => [a] -> [a] -> Bool
differenceProp xs ys = (HS.toList (HS.difference (HS.fromList xs) (HS.fromList ys))) == (toList (difference (fromList xs) (fromList ys)))

memberProp :: (Ord a) => a -> [a] -> Bool
memberProp x xs = (HS.member x (HS.fromList xs)) == (member x (fromList xs))

cardinalityProp :: (Ord a) => [a] -> Bool
cardinalityProp xs = (HS.size (HS.fromList xs)) == (cardinality (fromList xs))

setmapProp1 :: (Num a, Ord a) => [a] -> Bool 
setmapProp1 xs = (HS.toList (HS.map (+1) (HS.fromList xs)) == toList (setmap (+1) (fromList xs)))

setmapProp2 :: (Num a, Ord a) => [a] -> Bool 
setmapProp2 xs = (HS.toList (HS.map (*(-3)) (HS.fromList xs)) == toList (setmap (*(-3)) (fromList xs)))

setmapProp3 :: (Num a, Ord a) => [a] -> Bool
setmapProp3 xs = (HS.toList (HS.map (>0) (HS.fromList xs)) == toList (setmap (>0) (fromList xs)))

setfoldrProp1 :: (Num a, Ord a) => [a] -> Bool
setfoldrProp1 xs = HS.foldr (+) 0 (HS.fromList xs) == setfoldr (+) 0 (fromList xs)

setfoldrProp2 :: [Bool] -> Bool
setfoldrProp2 xs = HS.foldr (&&) True (HS.fromList xs) == setfoldr (&&) True (fromList xs)

setfoldrProp3 :: (Num a, Ord a) => [a] -> Bool
setfoldrProp3 xs = HS.foldr max 18 (HS.fromList xs) == setfoldr max 18 (fromList xs)

removeSetProp :: (Ord a) => a -> [a] -> Bool
removeSetProp x xs = (HS.toList (HS.delete x (HS.fromList xs))) == (toList (removeSet x (fromList xs)))

powerSetProp :: [Int] -> Bool
powerSetProp xs = (map (HS.toList) (HS.toList (HS.powerSet (HS.fromList $ xs)))) == (toList (setmap (toList) (powerSet (fromList xs))))

goodCheckProp :: IO ()
goodCheckProp =
   quickCheck (withMaxSuccess 10000 ((\x xs -> 
   (foldr (+) 0 (map (*10) ( HS.toList (foldr (\x y -> HS.delete x y) (HS.insert x (HS.fromList $ xs)) (take 500 xs)))) ) == (setfoldr (+) 0 (setmap (*10) (fromList (toList (foldr (\x y -> removeSet x y) (insert x (fromList $ xs)) (take 500 xs)))))) ) :: Int -> [Int] -> Bool))


a1 = (insert 21 (insert 20 (insert 2 (singleton 3))))
a2 = (insert 11 (insert 12345 (insert 1 (insert 5 (insert 2 (singleton 7))))))
a3 = (insert 7 (insert 11 (insert 1 (insert 11 (insert 12345 (insert 1 (insert 5 (insert 2 (singleton 7)))))))))

test1 = TestCase (assertEqual "empty test 1" False (member 3 empty))
test2 = TestCase (assertEqual "singleton test 1" True (member 3 (singleton 3)))
test3 = TestCase (assertEqual "singleton test 2" [7] (toList (singleton 7)))
test4 = TestCase (assertEqual "singleton test 3" False (member 3 (singleton 7)))
test5 = TestCase (assertEqual "member test 1" True (member 3 a1))
test6 = TestCase (assertEqual "member test 2" False (member 7 a1))
test7 = TestCase (assertEqual "member test 3" True (member 7 a3))
test8 = TestCase (assertEqual "toList test 1" [2,3,20,21] (toList a1))
test9 = TestCase (assertEqual "toList test 2" [1,2,5,7,11,12345] (toList a2))
test10 = TestCase (assertEqual "toList test 3" [1,2,5,7,11,12345] (toList a3))
test11 = TestCase (assertEqual "union test 1" [1,2,3,5,7,11,20,21,12345] (toList (union a1 a3)))
test12 = TestCase (assertEqual "union test 2" [1,2,5,7,11,12345] (toList (union a2 a2)))
test13 = TestCase (assertEqual "union test 3" [1,2,5,7,11,321,12345] (toList (union a2 (singleton 321))))
test14 = TestCase (assertEqual "intersection test 1" [1,2,5,7,11,12345] (toList (intersection a2 a3)))
test15 = TestCase (assertEqual "intersection test 2" [2] (toList (intersection a1 a2)))
test16 = TestCase (assertEqual "intersection test 3" [2,3,20,21] (toList (intersection a1 a1)))
test17 = TestCase (assertEqual "difference test 1" [] (toList (difference a2 a3)))
test18 = TestCase (assertEqual "difference test 2" [3,20,21] (toList (difference a1 a2)))
test19 = TestCase (assertEqual "difference test 3" [] (toList (difference a1 a1)))
test20 = TestCase (assertEqual "fromList test 1" True (member 3 (fromList [1,2,3,4,5])))
test21 = TestCase (assertEqual "fromList test 2" [1,2,3,4,7] (toList (union (fromList [1,2,3]) (fromList [3,4,7]))))
test22 = TestCase (assertEqual "fromList test 3" [7] (toList (intersection (fromList [1,2,7]) (fromList [7,8,9]))))
test23 = TestCase (assertEqual "isNull test 1" True (isNull (empty)))
test24 = TestCase (assertEqual "isNull test 2" True (isNull (difference a2 a3)))
test25 = TestCase (assertEqual "isNull test 3" False (isNull (intersection (fromList [1,2,7]) (fromList [7,8,9]))))
test26 = TestCase (assertEqual "insert test 1" True (member 20 (insert 20 (fromList [1,2,7]))))
test27 = TestCase (assertEqual "insert test 2" [7,8,9,42] (toList (insert 42 (fromList [7,8,9]))))
test28 = TestCase (assertEqual "insert test 3" False (isNull (insert 42 empty)))
test29 = TestCase (assertEqual "cardinality test 1" 0 (cardinality empty))
test30 = TestCase (assertEqual "cardinality test 2" 6 (cardinality a3))
test31 = TestCase (assertEqual "cardinality test 3" 7 (cardinality (fromList [1,2,3,17,18,19,42])))
test32 = TestCase (assertEqual "setmap test 1" [1] (toList (setmap (\_ -> 1) a1)))
test33 = TestCase (assertEqual "setmap test 2" [3,4,21,22] (toList (setmap (+1) a1)))
test34 = TestCase (assertEqual "setmap test 3" [True] (toList (setmap (\x -> member x a1) a1)))
test35 = TestCase (assertEqual "setfoldr test 1" 12371 (setfoldr (+) 0 a3))
test36 = TestCase (assertEqual "setfoldr test 2" 460 (setfoldr (\x y -> y + 10*x) 0 a1))
test37 = TestCase (assertEqual "setfoldr test 3" True (setfoldr (\x y -> y && member x a1) True a1))
test38 = TestCase (assertEqual "equality operator test 1" True (a2 == a3))
test39 = TestCase (assertEqual "equality operator test 2" False (a1 == a3))
test40 = TestCase (assertEqual "equality operator test 3" True (a2 == (fromList [7, 5, 12345, 2, 1, 11])))
test41 = TestCase (assertEqual "removeSet test 1" False (member 3 (removeSet 3 a1)))
test42 = TestCase (assertEqual "removeSet test 2" [2,20,21] (toList . removeSet 3 $ a1))
test43 = TestCase (assertEqual "removeSet test 3"  [1,3,20,21,33] (toList . removeSet 2 . insert 1 . insert 33 $ a1))
test44 = TestCase (assertEqual "removeSet test 4" True (isNull (foldr (\x y -> removeSet x y) a1 [2,3,20,21])))
test45 = TestCase (assertEqual "removeSet test 5" [1,2,5,12345] (toList ((foldr (\x y -> removeSet x y) a2 [11,7]))))
test46 = TestCase (assertEqual "powerSet test 1" [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]] (toList . setmap toList . powerSet . fromList $ [1,2,3]))

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6,
                  TestLabel "test7" test7,
                  TestLabel "test8" test8,
                  TestLabel "test9" test9,
                  TestLabel "test10" test10,
                  TestLabel "test10" test11,
                  TestLabel "test12" test12,
                  TestLabel "test13" test13,
                  TestLabel "test14" test14,
                  TestLabel "test15" test15,
                  TestLabel "test16" test16,
                  TestLabel "test17" test17,
                  TestLabel "test18" test18,
                  TestLabel "test19" test19,
                  TestLabel "test20" test20,
                  TestLabel "test21" test21,
                  TestLabel "test22" test22,
                  TestLabel "test23" test23,
                  TestLabel "test24" test24,
                  TestLabel "test25" test25,
                  TestLabel "test26" test26,
                  TestLabel "test27" test27,
                  TestLabel "test28" test28,
                  TestLabel "test29" test29,
                  TestLabel "test30" test30,
                  TestLabel "test31" test31,
                  TestLabel "test32" test32,
                  TestLabel "test33" test33,
                  TestLabel "test34" test34,
                  TestLabel "test35" test35,
                  TestLabel "test36" test36,
                  TestLabel "test37" test37,
                  TestLabel "test38" test38,
                  TestLabel "test39" test39,
                  TestLabel "test40" test40,
                  TestLabel "test41" test41,
                  TestLabel "test42" test42,
                  TestLabel "test43" test43,
                  TestLabel "test44" test44,
                  TestLabel "test45" test45,
                  TestLabel "test46" test46]

run = runTestTT tests




