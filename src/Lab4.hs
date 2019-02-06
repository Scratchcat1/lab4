--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 4: User-defined types                                                  --
--------------------------------------------------------------------------------

module Lab4 where

--------------------------------------------------------------------------------
-- Red-black trees

data Colour = Red | Black

instance Show Colour where
    show Red   = "Red"
    show Black = "Black"

data Tree a = Leaf | Node Colour (Tree a) a (Tree a)
    deriving Show

empty :: Tree a
empty = Leaf

singleton :: a -> Tree a
singleton x = Node Black Leaf x Leaf

makeBlack :: Tree a -> Tree a
makeBlack (Node c l x r) = Node Black l x r

depth :: Tree a -> Int
depth Leaf = 0
depth (Node c l x r) = maximum [depth l, depth r] + 1

toList :: Tree a -> [a]
toList Leaf = []
toList (Node c l x r) = (toList l) ++ [x] ++ (toList r)

member :: Ord a => a -> Tree a -> Bool
member val Leaf = False
member val (Node c l x r)
    | val == x = True
    | val <  x = member val l
    | val >  x = member val r 


balance :: Colour -> Tree a -> a -> Tree a -> Tree a
balance Black (Node Red (Node Red a x b) y c) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) =
    Node Red (Node Black a x b) y (Node Black c z d)
balance c l x r = Node c l x r

insertHelper :: Ord a => Tree a -> a -> Tree a
insertHelper Leaf val = (Node Red Leaf val Leaf)
insertHelper (Node c l x r) val
    | val == x = (Node c l x r)
    | val <  x = (balance c (insertHelper l val) x r)
    | val >  x = (balance c l x (insertHelper r val))

insert :: Ord a => Tree a -> a -> Tree a
insert tree val = (Node Black l x r)
    where
        (Node _ l x r) = insertHelper tree val

--------------------------------------------------------------------------------
