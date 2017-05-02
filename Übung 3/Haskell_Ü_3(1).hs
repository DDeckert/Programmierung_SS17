data Tree = Leaf Int | Branch Tree Tree

--Aufgabe 1
leaves_count :: Tree -> Int
leaves_count (Leaf _) = 1
leaves_count (Branch left right) = leaves_count left + leaves_count right

leaf_list :: Tree -> [Int]
leaf_list (Leaf x) = [x]
leaf_list (Branch left right) = leaf_list left ++ leaf_list right












