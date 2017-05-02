-- Aufgabe 1
fac :: Int -> Int
fac 0 = 1 
fac n = n * fac (n-1)

sumFacs :: Int -> Int -> Int
sumFacs n m = if (n > m)
	then 0
	else fac n + sumFacs (n+1) m
	
sumFacs' :: Int -> Int -> Int
sumFacs' n m 
	|n<= m = fac n + sumFacs' (n+1) m
	|otherwise = 0

-- Aufgabe 2	
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

prod :: [Int] -> Int 
prod [] = 1
prod (x:y:xs) = x * y * prod xs
prod (x:xs) = x * prod xs

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

rem' :: Int -> [Int] -> [Int]
rem' _ [] = []
rem' n (m:ms) 
	|m == n = rem' n ms
	|otherwise = [m] ++ (rem' n ms)

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True
isOrd (x:y:xs) 
	|x<=y = isOrd (y:xs)
	|otherwise = False
isOrd (x:y:xs) = (x<=y) && isOrd (y:xs)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	|x<y = x: (merge xs (y:ys))
	|otherwise = y: (merge (x:xs) ys))

