pack :: [Char] -> [[Char]]
pack [] = []
pack (x:xs) = aux xs [x]
	where
		aux [] ys = [ys]
		aux [x] (y:ys) =if x==y then [(x:y:ys)] else (y:ys): [[x]]
		aux (x1:x2:xs) (y:ys)
			|x1 == y =aux (x2:xs) (x1:y:ys)
			|otherwise = (y:ys) : (aux (x2:xs) [x1])
			
