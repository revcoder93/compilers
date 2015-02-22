---------------------------------------------------------
--Courtesy of CSEN 403 Concepts of Programming languages
--Prof. Dr. Slim Abdennadher & Eng. Maha Badreldin
---------------------------------------------------------

double x = x + x

quadruple x = double (double x)

twice x = 2 * x

square x = x * x

abs x = if x >= 0 then x else -x

signum x = if x < 0 then - 1 else
	if x==0 then 0 else 1
	
abs1 x | x >= 0 = x
	   | otherwise = -x
	  
signum1 x | x < 0 = -1
		  | x == 0 = 0
		  | otherwise = 1
		 
fact n = if n==0 then 1 else
			fact (n-1) * n
			
fact1 0 = 1
fact1 n = fact (n-1) * n

even1 :: Integer -> Bool
even1 x | mod x 2 == 0 = True
		| otherwise = False
		
add :: Int -> (Int -> Int)
add x y = x + y
succ x = x + 1

add1 :: Int -> Int -> Int
add1 x y = x + y
succ1 = add1 1

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

mult1 :: Int -> Int -> Int -> Int
mult1 x y z = x * y * z

data Fruit = Kiwi | Apple | Banana
isYellow :: Fruit -> Bool
isYello Kiwi = False
isYello Apple = False
isYellow Banana = True

type Point = (Int,Int)
getX :: Point -> Int
getX (x,y) = x

data Point1 = Pt (Int,Int)
getX1 :: Point1 -> Int
getX1 (Pt (x,y)) = x

data Nat = Zero | Succ Nat deriving Show
natPlus Zero n = n
natPlus (Succ m) n = Succ (natPlus m n)

data List a = Nil | Cons a (List a) deriving Show
length1 :: List a -> Int
length1 Nil = 0
length1 (Cons x y) = 1 + length1 y

sumAll [] = 0
sumAll (x:xs) = x + sumAll xs

doubleAll [] = []
doubleAll (x:xs) = (x*2) : doubleAll xs

union1 x y = x ++ y

getNth x y = x !! y

length2 [] = 0
length2 (x:xs) = 1 + length xs

concat1 [] = []
concat1 (l:ls) = l ++ concat1 ls

head1 [] = error "head1 []"
head1 (x:xs) = x

tail1 [] = error "tail1 []"
tail1 (x:xs) = xs

last1 [] = error "last []"
last1 [x] = x
last1 (x:xs) = last1 xs

zip1 xs [] = []
zip1 [] ys = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

unzip1 [] = ([], [])
unzip1 ((x,y):ps) = (x:l1, y:l2) where (l1,l2) = unzip1 ps

seq1 x y = [x..y]
seq2 x y = [x,x+2..y]
seqb x y = [x,x-1..y]
seqb2 x y = [x,x-2..y]
seqs x y z = [x,x+z..y]
seqbs x y z = [x,x-z..y]

factAll [] = []
factAll (x:xs) = fact x : factAll xs

map1 f [] = []
map1 f (x:xs) = (f x) : map1 f xs
factAll1 xs = map1 fact xs

product1 [] = 1
product1 (x:xs) = x * product1 xs

concat2 [] = []
concat2 (l:ls) = l ++ concat2 ls

foldr2 f e [] = e
foldr2 f e (x : xs) = f x (foldr2 f e xs)
product3 list = foldr2 (*) 1 list
concat3 list = foldr2 (++) [] list

addOnetoList l = map1 (add1 1) l

gt :: Ord a => a -> a -> a
gt x y = if x < y then y else x