data Square = Black | White
    deriving Eq
flipSquare :: Square -> Square
flipSquare Black = White
flipSquare White = Black

type Coor = (Int, Int)

instance Show Square where
    show White = " "
    show Black = "█"

instance Show Board where
    show (Board x) = foldr (\x y-> show x ++ show x ++ y) "\n" (take 3 x) ++
        foldr (\x y-> show x ++ show x ++ y) "\n" (take 3 (drop 3 x)) ++
        foldr (\x y-> show x ++ show x ++ y) "\n" (take 3 (drop 6 x))

data Board = Board [Square]
get ::Board -> Coor -> Square
get (Board grid) (x, y) = grid !! (x + y * 3)

set :: Board -> Coor -> Square -> Board
set (Board grid) (x, y) color 
    | x < 0 || x > 2 = (Board grid)
    | y < 0 || y > 2 = (Board grid)
    | otherwise = Board $ take c grid ++ color:drop (c + 1) grid 
    where
        c = (x + y * 3)

flipOne :: Board -> Coor -> Board
flipOne board coor = set board coor $ flipSquare $ get board coor

coinFlip :: Board -> Coor -> Board
coinFlip board (x, y) = foldr (\c b -> flipOne b c) board ([(x1,y1) | x1 <-[x-1..x+1], y1 <- [y-1..y+1], x == x1 || y == y1])

complete :: Board
complete = Board $ replicate 9 Black

book :: Board
book = foldr (\x y -> flipOne y x) complete [(1,0), (2,0), (0,2), (2,2)]

isDone :: Board -> Bool
isDone (Board grid) = all (== Black) grid

allCoor :: [Coor]
allCoor = [(x,y) | x <- [0..2] , y <- [0..2]]

--solve :: [Board] -> String
--solve :: (Ord a, Num a, Show a) => [([Coor], Board)] -> [Char]
solve :: [([Coor], Board)] -> [Char]
solve boards
    |depth >= 10 = "Max Depth"
    |isDone board = show coors
    |otherwise = solve (drop 1 boards ++ map (\x -> (x:coors, coinFlip board x)) allCoor)
    where
        depth = length $ fst $ boards !! 0
        board = snd $ boards !! 0
        coors = fst $ boards !! 0