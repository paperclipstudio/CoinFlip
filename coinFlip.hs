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
    show (Board xs width _) = foldr (\x y -> x ++ ['\n'] ++ y) "" $ map (\x -> foldr (\x y-> show x ++ show x ++ y) "" x)  $ split width xs 


split :: Int -> [Square] -> [[Square]]

split n board 
    | length board <= n = [board]
    | otherwise = [(take n board)] ++ (split n (drop n board))

data Board = Board [Square] Int Int

width :: Board -> Int
width (Board _ width _) = width

height :: Board -> Int
height (Board _ _ height) = height

get ::Board -> Coor -> Square
get (Board grid width height) (x, y)
    | x < 0 || x >= width = White
    | y < 0 || y >= height = White
    | otherwise = grid !! (x + y * width)

set :: Board -> Coor -> Square -> Board
set board@(Board grid width height) (x, y) color 
    | x < 0 || x >= width = board
    | y < 0 || y >= height = board
    | otherwise = Board (take c grid ++ color:drop (c + 1) grid) width height 
    where
        c = (x + y * width)

flipOne :: Board -> Coor -> Board
flipOne board coor = set board coor $ flipSquare $ get board coor

coinFlip :: Board -> Coor -> Board
coinFlip board (x, y) = foldr (\c b -> flipOne b c) board ([(x1,y1) | x1 <-[x-1..x+1], y1 <- [y-1..y+1], x == x1 || y == y1])

complete :: Board
complete = Board (replicate 9 Black) 3 3

book :: Board
book = foldr (\x y -> flipOne y x) complete [(1,0), (2,0), (0,2), (2,2)]

isDone :: Board -> Bool
isDone (Board grid _ _) = all (== Black) grid

allCoor :: Board -> [Coor]
allCoor board = [(x,y) | x <- [0..((width board) - 1)] , y <- [0..((height board) - 1)]]

--solve :: [Board] -> String
--solve :: (Ord a, Num a, Show a) => [([Coor], Board)] -> [Char]
solve :: [([Coor], Board)] -> [Char]
solve [] = "nope"
solve ((coors,board):boards)
    |depth >= 5 = "Max Depth"
    |isDone board = show coors
    |solve boards /= "nope" = solve boards
    |otherwise = solve (map (\x -> (x:coors, coinFlip board x)) (allCoor board))
    where
        depth = length $ coors