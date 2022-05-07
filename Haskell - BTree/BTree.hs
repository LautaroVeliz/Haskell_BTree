import Tools
data BTree a = Nil  | Leaf Int [a] | Node Int [a] [BTree a] deriving (Show)

-- Crear desde un [a] con la funcion btree que usa insert (la cual a su vez fusiona de ser necesario con fuseBTree) --
btree :: (Ord a) => Int -> [a] -> BTree a
btree n xs
    |   n < 2 = Nil
    |   xs == [] = Leaf n []
    |   otherwise = foldl (\acc x -> insert acc x) bt xs
    where bt = Leaf n []

insert :: (Ord a) => BTree a -> a -> BTree a
insert Nil _ = error "Cant use a Nil B-Tree"
insert (Leaf n []) val = Leaf n [val]
insert (Leaf n xs) val = if length orderList <= n 
                         then Leaf n $ orderList 
                         else Node n [elementAt orderList $ takeElem +1] $ 
                         (Leaf n $ take takeElem orderList):[Leaf n $ drop (takeElem +1) orderList]
    where   orderList = ordenaPorSeleccion (val:xs)
            takeElem = div (n+1) 2
insert (Node n xs bts) val = fuseBTree (Node n xs bts) (insert (elementAt bts sonPos) val) sonPos
    where   sonPos = foldl(\acc x -> if val > x then acc+1 else acc) 1 xs

fuseBTree :: (Ord a) => BTree a -> BTree a -> Int -> BTree a
fuseBTree b1 Nil _ = b1
fuseBTree Nil b2 _ = b2
fuseBTree (Leaf n (x:xs)) _ _ = error "The fusion of a BTree into an Leaf is imposible"
fuseBTree (Node m xs xbts) (Node n ys ybts) i = if showDeep (head xbts) == showDeep (head ybts)
                                                    then if length orderList <= m 
                                                        then newSimpleNode
                                                        else Node m [elementAt orderList $ takeElem +1] $ 
                                                        (Node m (take (takeElem) orderList) $ take (takeElem +1) sonsList):
                                                        [Node m (drop (takeElem +1) orderList) $ drop (takeElem + 1) sonsList]
                                                    else Node m xs $ replace xbts i (Node n ys ybts)
    where   newSimpleNode = Node m orderList $ sonsList
            orderList = ordenaPorSeleccion ((head ys):xs)
            sonsList = (take (i-1) xbts)++ybts++(drop i xbts)
            takeElem = div (m+1) 2
fuseBTree (Node m xs xbts) (Leaf n ys) i = Node m xs $ replace xbts i (Leaf n ys)

-- Mostrar la profundida --
showDeep :: BTree a -> Int
showDeep (Node n xs (bt:bts)) = 1 + showDeep bt
showDeep (Leaf n xs) = 1
showDeep Nil = 0

-- Buscar el maximo --
findMax :: (Ord a) => BTree a -> a
findMax Nil = error "Cannot get a max value from a Nil BTree"
findMax (Leaf n []) = error "Cannot get a max value from an empty Leaf"
findMax (Leaf n xs) = foldl1 (\acc x -> if x > acc then x else acc) xs
findMax (Node n xs bts) = findMax $ last bts

-- Buscar el minimo --
findMin :: (Ord a) => BTree a -> a
findMin Nil = error "Cannot get a min value from a Nil BTree"
findMin (Leaf n []) = error "Cannot get a min value from an empty Leaf"
findMin (Leaf n xs) = foldl1 (\acc x -> if x < acc then x else acc) xs
findMin (Node n xs bts) = findMin $ head bts

-- Devolver un array de [a] --
elemsArray :: BTree a -> [a]
elemsArray Nil = error "Cannot get elements from a Nil BTree"
elemsArray (Leaf n []) = []
elemsArray (Leaf n xs) = xs
elemsArray (Node n xs bts) = xs ++ foldl (\acc bt -> (elemsArray bt)++acc) [] bts

-- Mostrar la cantidad de item en el arbol --
treeLength :: BTree a -> Int
treeLength Nil = 0
treeLength (Leaf n []) = 0
treeLength (Leaf n xs) = length xs
treeLength (Node n xs bts) = length xs + foldl (\acc bt -> treeLength bt + acc) 0 bts

-- Imprimir inoorder --
inOrder :: (Eq a) => BTree a -> [a]
inOrder Nil = error "Cannot get elements from a Nil BTree"
inOrder (Leaf n []) = []
inOrder (Leaf n xs) = xs
inOrder (Node n xs bts) = (foldl (\acc x -> acc++(inOrder $ elementAt bts $ indexOf xs x)++ [x]) [] xs) ++ (inOrder $ last bts)
 
-- Imprimir preorder --
preorder :: BTree a -> [a]
preorder Nil = error "Cannot get elements from a Nil BTree"
preorder (Leaf n []) = []
preorder (Leaf n xs) = xs
preorder (Node n xs bts) = xs ++ (foldl (\acc bt -> acc++preorder bt) [] bts)

-- Imprimir postorder --
postorder :: BTree a -> [a]
postorder Nil = error "Cannot get elements from a Nil BTree"
postorder (Leaf n []) = []
postorder (Leaf n xs) = xs
postorder (Node n xs bts) = (foldl (\acc bt -> acc++postorder bt) [] bts) ++ xs