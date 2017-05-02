unwords' :: [String] -> String
unwords' [] 		= []
unwords' (s : xs) 	= s ++ " " ++ (unwords' xs)

words' :: String -> [String]
words' xs = aux xs ""
	where 
		aux [] ys = [ys]
		aux (x:xs) ys
			| x == ' ' && ys == [] = aux xs ""
			| x == ' ' = ys : (aux xs "")
			|otherwise = aux xs (ys ++ [x])

max_length :: [[Int]] -> Int
max_length [] 		= 0
max_length (x:xs) 	= max (length x) (max_length xs)

max_length' :: [[Int]] -> Int 
max_length' xs = foldr (max) 0 (map (length) xs)






