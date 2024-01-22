--Ekin Tasyure 150190108
import Data.Maybe (listToMaybe, isNothing)
import Data.List (maximumBy)
import Data.Ord (comparing)

--
--Part 1 - Module Design
--
-- Define chess pieces and teams
data Piece = Bishop | Knight | Rook | Queen | Pawn deriving (Eq, Show)
data Team = Red | Green | Blue | Purple | White deriving (Eq, Show)
type Position = (Int, Int)
type Chessboard = [[Maybe (Piece, Team)]]

--
-- Part 2 - Score and Change Functions
--

-- Attack Count
-- Calculate the total number of attacks on the board
attackCount :: Chessboard -> Int
attackCount board = sum [ countAttacks piece board | row <- board, Just piece <- row ]

-- Count attacks for a specific piece on the board
countAttacks :: (Piece, Team) -> Chessboard -> Int
countAttacks piece board = length $ filter (\defender -> isAttackableBy piece defender board) (concat board)

-- Find the position of a specific piece on the board
findPosition :: (Piece, Team) -> Chessboard -> Maybe Position
findPosition piece board =
    let positions = [ (x, y) | (x, row) <- zip [0..] board, (y, Just p) <- zip [0..] row, p == piece ]
    in listToMaybe positions

-- Generate the path between two positions on the board
generatePath :: Position -> Position -> [Position]
generatePath (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 + 1 .. max y1 y2 - 1]]  -- Vertical movement
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 + 1 .. max x1 x2 - 1]]  -- Horizontal movement
  | otherwise = zip (range x1 x2) (range y1 y2)                   -- Diagonal movement
  where
    range a b = [a, a + signum (b - a) .. b]

-- Check if the path between two positions is clear of other pieces
isPathClear :: [Position] -> Chessboard -> Bool
isPathClear path board = all (\pos -> isNothing (getPieceAt pos board)) path

-- Get the piece at a specific position on the board
getPieceAt :: Position -> Chessboard -> Maybe (Piece, Team)
getPieceAt (x, y) board = if inBounds (x, y) board then (board !! x) !! y else Nothing

-- Check if a position is within the bounds of the board
inBounds :: Position -> Chessboard -> Bool
inBounds (x, y) board = x >= 0 && y >= 0 && x < length board && y < length (head board)

-- Determine if a piece can attack another piece
isAttackableBy :: (Piece, Team) -> Maybe (Piece, Team) -> Chessboard -> Bool
isAttackableBy _ Nothing _ = False
isAttackableBy attackingPiece@(attackingType, _) (Just (defendingPiece, defendingTeam)) board
    | snd attackingPiece == defendingTeam = False  -- Cannot attack same team
    | otherwise =
        case findPosition attackingPiece board of
            Nothing -> False
            Just pos -> canAttack attackingType pos (findPosition (defendingPiece, defendingTeam) board) board

-- Determine if a piece can attack a position
canAttack :: Piece -> Position -> Maybe Position -> Chessboard -> Bool
canAttack Pawn posAttacker posDefender _ = canPawnAttack posAttacker posDefender
canAttack Knight posAttacker posDefender _ = canKnightAttack posAttacker posDefender
canAttack Bishop posAttacker posDefender board = canBishopAttack posAttacker posDefender board
canAttack Rook posAttacker posDefender board = canRookAttack posAttacker posDefender board
canAttack Queen posAttacker posDefender board = canQueenAttack posAttacker posDefender board

-- Pawn attack logic
canPawnAttack :: Position -> Maybe Position -> Bool
canPawnAttack (x, y) (Just (dx, dy)) =
    dx == x + 1 && (dy == y + 1 || dy == y - 1) ||
    dx == x - 1 && (dy == y + 1 || dy == y - 1)
canPawnAttack _ _ = False

-- Knight attack logic
canKnightAttack :: Position -> Maybe Position -> Bool
canKnightAttack (x, y) (Just (dx, dy)) =
    (abs (x - dx) == 2 && abs (y - dy) == 1) || (abs (x - dx) == 1 && abs (y - dy) == 2)
canKnightAttack _ _ = False

-- Bishop attack logic
canBishopAttack :: Position -> Maybe Position -> Chessboard -> Bool
canBishopAttack posAttacker (Just posDefender) board =
    abs (fst posAttacker - fst posDefender) == abs (snd posAttacker - snd posDefender) &&
    isPathClear (generatePath posAttacker posDefender) board
canBishopAttack _ _ _ = False

-- Rook attack logic
canRookAttack :: Position -> Maybe Position -> Chessboard -> Bool
canRookAttack posAttacker (Just posDefender) board =
    (fst posAttacker == fst posDefender || snd posAttacker == snd posDefender) &&
    isPathClear (generatePath posAttacker posDefender) board
canRookAttack _ _ _ = False

-- Queen attack logic
canQueenAttack :: Position -> Maybe Position -> Chessboard -> Bool
canQueenAttack posAttacker posDefender board =
    canRookAttack posAttacker posDefender board || canBishopAttack posAttacker posDefender board

--BoardValue
pieceValue :: Piece -> Float
pieceValue Bishop = 3.25
pieceValue Knight = 3.25
pieceValue Rook = 5
pieceValue Queen = 10
pieceValue Pawn = 1

-- Calculate the board value based on the values of the pieces
boardValue :: Chessboard -> Float
boardValue board = sum [ pieceValue p | row <- board, Just (p, _) <- row ]

--Horizontal Crossover
horizontalCrossover :: Int -> Chessboard -> Chessboard
horizontalCrossover line board = let (upper, lower) = splitAt line board
                                 in lower ++ upper

--Vertical Crossover
verticalCrossover :: Int -> Chessboard -> Chessboard
verticalCrossover line board = [ left ++ right | row <- board, let (left, right) = splitAt line row ]

--Delete Piece
deletePiece :: Position -> Chessboard -> Chessboard
deletePiece (x, y) board =
    take x board ++
    [take y (board !! x) ++ [Nothing] ++ drop (y + 1) (board !! x)] ++
    drop (x + 1) board

--
-- Part 3 - Chessboard Tree
--

-- Chessboard node for building the tree
data ChessboardNode = ChessboardNode {
    board :: Chessboard,
    depth :: Int
} deriving (Eq, Show)

-- Generate branches for horizontal crossover
generateHorizontalCrossoverBranches :: ChessboardNode -> [ChessboardNode]
generateHorizontalCrossoverBranches node =
    [ ChessboardNode (horizontalCrossover i (board node)) (depth node + 1) | i <- [1 .. length (board node) - 1]]

-- Generate branches for vertical crossover
generateVerticalCrossoverBranches :: ChessboardNode -> [ChessboardNode]
generateVerticalCrossoverBranches node =
    let boardWidth = length (head (board node))
    in [ ChessboardNode (verticalCrossover i (board node)) (depth node + 1) | i <- [1 .. boardWidth - 1]]

-- Generate branches for deleting a piece
generateDeletePieceBranches :: ChessboardNode -> [ChessboardNode]
generateDeletePieceBranches node =
    [ ChessboardNode (deletePiece (x, y) (board node)) (depth node + 1)
    | x <- [0 .. length (board node) - 1], y <- [0 .. length (head (board node)) - 1]]

-- Build the tree of chessboards to a given depth
buildTree :: ChessboardNode -> Int -> [ChessboardNode]
buildTree node maxDepth
    | depth node >= maxDepth = [node]
    | otherwise = node : concatMap (`buildTree` maxDepth) (generateBranches node)
    where
        generateBranches n = generateHorizontalCrossoverBranches n
                             ++ generateVerticalCrossoverBranches n
                             ++ generateDeletePieceBranches n

-- Calculate the value of a team on the board
teamValue :: Team -> Chessboard -> Float
teamValue team board = sum [ pieceValue p | row <- board, Just (p, t) <- row, t == team ]

-- Evaluate the board based on the number of attacks and the value of the weakest team
evaluateBoard :: Chessboard -> Float
evaluateBoard board = teamValueFactor * minimumTeamValue - attackCountFactor * totalAttacks
  where
    totalAttacks = fromIntegral (attackCount board)
    minimumTeamValue = minimum (map (`teamValue` board) [Red, Green, Blue, Purple, White])
    teamValueFactor = 1.0
    attackCountFactor = 1.0

-- Find the best board configuration at each level of the tree
findBestBoardAtEachLevel :: ChessboardNode -> Int -> [(Int, Chessboard)]
findBestBoardAtEachLevel rootNode maxDepth = 
    map bestBoardAtLevel [0 .. maxDepth]
    where
        tree = buildTree rootNode maxDepth
        bestBoardAtLevel d = 
            let boardsAtLevel = filter (\n -> depth n == d) tree
                evaluatedBoards = map (\n -> (evaluateBoard (board n), board n)) boardsAtLevel
            in (d, snd $ maximumBy (comparing fst) evaluatedBoards)

--
-- Testing
--

startBoard :: Chessboard
startBoard =
  [ [Nothing, Nothing, Nothing, Just (Bishop, White), Just (Knight, Purple) ,Nothing, Nothing]
  , [Nothing, Nothing, Just (Queen, Purple), Nothing, Nothing, Nothing, Just (Knight, Purple), Nothing]
  , [Nothing, Nothing, Nothing, Just (Knight, White), Nothing, Just (Rook, Green), Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Just (Queen, Blue), Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Knight, Blue), Nothing]
  , [Nothing, Nothing, Just (Queen, Red), Nothing, Just (Rook, Green), Just (Bishop, White), Nothing, Nothing]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Just (Bishop, White), Nothing, Nothing]
  ]

-- Root node for the chessboard tree
rootNode :: ChessboardNode
rootNode = ChessboardNode startBoard 0

-- Test functions to build the tree and find the best boards
testTree :: [ChessboardNode]
testTree = buildTree rootNode 5

testBestBoards :: [(Int, Chessboard)]
testBestBoards = findBestBoardAtEachLevel rootNode 5

main :: IO ()
main = do
    let testBestBoards = findBestBoardAtEachLevel rootNode 5
    putStrLn "Best Boards at Each Level with Metrics: "
    sequence_ [printLevel level board | (level, board) <- testBestBoards]

printLevel :: Int -> Chessboard -> IO ()
printLevel level board = do
    let attacks = attackCount board
    let teamValues = map (\team -> (team, teamValue team board)) [Red, Green, Blue, Purple, White]
    putStrLn $ "Level " ++ show level
    putStrLn $ "Total Number of Attacks: " ++ show attacks
    putStrLn "Team Values: "
    mapM_ (\(team, value) -> putStrLn $ "  " ++ show team ++ ": " ++ show value) teamValues
    putStrLn ""



