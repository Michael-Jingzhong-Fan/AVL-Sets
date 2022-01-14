module Coursework where

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
         apples = tail carrot
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








