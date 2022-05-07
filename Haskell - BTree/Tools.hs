module Tools
( ordenaPorSeleccion
, elementAt
, replace
, indexOf
) where

ordenaPorSeleccion :: Ord a => [a] -> [a]
ordenaPorSeleccion [] = []
ordenaPorSeleccion xs = m : ordenaPorSeleccion (delete m xs)
    where m = minimum xs 

delete :: Ord a => a -> [a] -> [a]
delete _ [] = []
delete val (x:xs) = if x == val
                  then delete val xs
                  else x:delete val xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Cant get an element from an empty list"
elementAt (x:xs) 0 = error "Can't get the zero element from a list"
elementAt (x:xs) i
    |   i < 0 = error "Can't get the a negative position of a list"
    |   i > length (x:xs) = error "Unreachable index"
    |   otherwise = if i == 1 then x else elementAt xs (i-1) 

replace :: [a] -> Int -> a -> [a]
replace [] _ _ = error "Cant replace in a empty list" 
replace (x:xs) n val
    |   n > (length xs +1) || n < 0 = error "Cant replace this position"
    |   otherwise = if n == 1 then val:xs else x:replace xs (n-1) val   

indexOf :: (Eq a) => [a] -> a -> Int 
indexOf (x:xs) val
    |   val `elem` (x:xs) = if val == x then 1 else 1 + indexOf xs val
    |   otherwise = 0