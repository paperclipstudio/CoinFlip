

data Square = Black | White
    deriving Eq
flipSquare :: Square -> Square
flipSquare Black = White
flipSquare White = Black

type Coor = (Int, Int)

instance Show Square where
    show White = "▒"
    show Black = "█"

instance Show Board where
    show (Board x width _) = "\n" ++ foldr (\x y-> show x ++ show x ++ y) "\n" (take width x) ++
        foldr (\x y-> show x ++ show x ++ y) "\n" (take width (drop width x)) ++
        foldr (\x y-> show x ++ show x ++ y) "\n" (take width (drop (width*2) x)) ++
        foldr (\x y-> show x ++ show x ++ y) "\n" (take width (drop (width*3) x)) ++ "\n"
data Board = Board [Square] Int Int

width :: Board -> Int
width (Board _ width _) = width

height :: Board -> Int
height (Board _ _ height) = height

get ::Board -> Coor -> Square
get (Board grid width _) (x, y) = grid !! (x + y * width)

set :: Board -> Coor -> Square -> Board
set board@(Board grid width height) (x, y) color 
    | x < 0 || x >= width = board
    | y < 0 || y >= height = board
    | otherwise = Board (take c grid ++ color:drop (c + 1) grid ) width height
    where
        c = (x + y * width)

flipOne :: Board -> Coor -> Board
flipOne board coor = set board coor $ flipSquare $ get board coor

coinFlip :: Board -> Coor -> Board
coinFlip board (x, y) 
    | x < 0 || x >= width board = board
    | y < 0 || y >= height board = board
    | otherwise = foldr (\c b -> flipOne b c) board ([(x1,y1) | x1 <-[x-1..x+1], y1 <- [y-1..y+1], x == x1 || y == y1])

complete :: Int -> Board
complete x = Board (replicate (x*x) Black) x x

book :: Board
book2 :: Board
book2 = foldr (\x y -> coinFlip y x) (complete 5) [(mod a 5,mod b 5) | a <- [1..12], b <- [4..19]]

book = foldr (\x y -> flipOne y x) (complete 3) [(1,0), (2,0), (0,2), (2,2)]

isDone :: Board -> Bool
isDone (Board grid _ _) = all (== Black) grid

allCoor :: Board -> [Coor]
allCoor (Board _ width height)= [(x,y) | x <- [0..(width-1)], y <- [0..(height-1)]]



data Queue a = Queue [a] [a]

enqueue :: Queue a -> a -> Queue a
enqueue (Queue ein eout) e = (Queue (e:ein) eout) 

dequeue :: Queue a -> (Queue a, a)
dequeue (Queue [] []) = error "can't dequeue empty"
dequeue (Queue ein []) = dequeue (Queue [] (reverse ein))
dequeue (Queue ein (e:out)) = ((Queue ein out), e)

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

test = (Queue [] [1])

instance (Show a) => Show (Queue a) where
    show (Queue ein eout) = show (eout ++ (reverse ein))


solveBoard :: Board -> String
solveBoard b = solve (Queue [] [([], b)])
--solve :: [Board] -> String
--solve :: (Ord a, Num a, Show a) => [([Coor], Board)] -> [Char]
solve :: (Queue ([Coor], Board)) -> [Char]
solve (q)
    |depth >= 10 = "Max Depth"
    |isDone board = show coors
    |otherwise = solve (foldr (\x y-> enqueue y x) nextQ $ nextBoards)
    where
        depth = length $ coors
        (nextQ, current) = dequeue q
        coors = fst current
        board = snd current
        lastMoveValue = if coors == [] then 9999 else moveValue board $ coors !! 0
        nextBoards = [(x:coors, coinFlip board x)| x <- (allCoor board), (moveValue board x) < lastMoveValue]

moveValue :: Board -> Coor -> Int
moveValue b (x, y) = x + y *(width b)