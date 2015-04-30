double x = x + x

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

even1 x | mod x 2 == 0 = True
		| otherwise = False

sumAll [] = 0
sumAll (x:xs) = x + sumAll xs

doubleAll [] = []
doubleAll (x:xs) = (x*2) : doubleAll xs

length2 [] = 0
length2 (x:xs) = 1 + length xs

concat1 [] = []
concat1 (l:ls) = l ++ concat1 ls

head1 [] = error "head1 []"
head1 (x:xs) = x

tail1 [] = error "tail1 []"
tail1 (x:xs) = xs

factAll [] = []
factAll (x:xs) = fact x : factAll xs

product1 [] = 1
product1 (x:xs) = x * product1 xs 