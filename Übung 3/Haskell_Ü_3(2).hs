data Tree a = Leaf a | Branch a (Tree a) (Tree a)

--Aufgabe 2
depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ left right) = 1 + min (depth left) (depth right)

paths :: Tree a -> Tree [a]
paths t = aux t []
	where
		aux (Leaf x) ys = Leaf (ys ++ [x])
		aux (Branch x left right) ys = Branch (ys ++ [x]) (aux left (ys++[x])) (aux right (ys++[x]))

		
--Aufgabe 3
f :: [Int] -> Int
f xs = foldr (*) 1 (map (square) (filter (even) xs))
{-
Alternativen:
	f = foldr (*) 1 . map (square) . filter even
	f = foldr (*) 1 . map (\x->x*x) . filter even
-}
	where square x = x * x

	
--Aufgabe 4 (Mit der oben definierten Tree-Datenstruktur)
tmap :: (a->b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf (f a)
tmap f (Branch a (left) (right)) = Branch (f a) (tmap f left) (tmap f right)