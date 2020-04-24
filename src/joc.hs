import System.Random
import Data.List

-- ==========================================================================================
-- Definition of types
-- ==========================================================================================

-- The board is a list of integers where each element of the list is a column of the game connect-4
type Board = [[Int]] 
data Player = Player1 | Bot
	deriving (Eq)
type Column = Int --Column of the board
type Strategy = Board  -> IO Column


-- ==========================================================================================
-- Auxiliar Functions ##General Section##
-- ==========================================================================================


-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).
randInt :: Int -> Int -> IO Int
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result


-- isNum is a function that checks whether all characters of a string are numbers between 1 and 3 both inclusive or not
isNum :: String -> Bool
isNum s = (length s == 1) && (all (\c -> c >= '1' && c <= '3') s) 


-- Given a string, validSize check whether the input of the size of the board is correct or not
-- The string should consist of 2 numbers, and at least one should be greater equal than 4
validSize :: String -> Bool
validSize s
	| (null s1) || (null s2) = False
	| not $ isNum s2 = False
	| (row < 4) && (col < 4) = False
	| otherwise = True
		where
			isNum x = all (\c -> c >= '0' && c <= '9') s2
			s1 = takeWhile (\c -> c >= '0' && c <= '9') s 
			s2 = dropWhile (\c -> c == ' ') (drop (length s1) s)
			row = read s1 :: Int
			col = read s2 :: Int



-- ==========================================================================================
-- Auxiliar Functions ##Board Section##
-- Assumption: The board given is always a valid representation of the board of connect-4
-- ==========================================================================================

-- Given a board, return the possible columns that the player can put its pieces
getValidColumns :: Board -> [Int]
getValidColumns board = (filter (\x -> last (board !! x) == 0) [0..((length board) - 1)])


-- Given a board, return the possible columns that the player can put its pieces prioritizing the middle columns
getValidColumns_v2 :: Board -> [Int]
getValidColumns_v2 board = sortBy (\x y -> if(abs(x-(div (length board) 2)) <= abs(y-(div (length board) 2))) then LT else GT) (filter (\x -> last (board !! x) == 0) [0..((length board) - 1)])


-- Given a board, return the board represented by rows
change_to_row :: Board -> Board
change_to_row = reverse . transpose


-- Helper function to paint the board
-- If player is bot the color is green
-- If player is human the color is red
paintLine :: [Int] -> String
paintLine [] = ""
paintLine (x:xs)
	| x == 0 = "\x1b[0m-" ++ paintLine xs
	| x == 1 = "\x1b[31mX" ++ paintLine xs
	| x == 2 = "\x1b[32mX" ++ paintLine xs


-- Helper function to paint the board
paintMatrix :: [[Int]] -> String
paintMatrix [] = ""
paintMatrix (x:xs) = (paintLine x) ++ "\n" ++ (paintMatrix xs) 


-- Given a board, return the representation of the board printed in the terminal
paintBoard :: Board -> String
paintBoard board = paintMatrix $ change_to_row board


-- Given an integer x and a list l , modify the last 0 in the list l for the value x. 
change_value :: Int -> [Int] -> [Int]
change_value x l = aux ++ [x] ++ aux1
	where
		aux = takeWhile (/= 0) l
		aux1 = drop ((length aux) + 1) l


-- Given a board, a player and a column Column pos, put its piece to the column pos of the board and return the modified board
-- Pre: the Column given is a valid column, player is either Bot or Player1
putPiece :: Board -> Player -> Column -> Board
putPiece b player pos
	| player == Player1 = concat [aux1 , (change_value 1 (b!!pos)):aux2]
	| otherwise =  concat [aux1 , (change_value 2 (b!!pos)):aux2]
		where
			aux1 = (take pos b)
			aux2 = (drop (pos + 1) b)


-- Given a board and two integers i and j, return the piece in board[i][j]
-- Pre: i j are valid Columns 
valueAt :: Board -> (Int,Int) -> Int
valueAt board (i,j) = board !! i !! j


-- Given two integers n and m, return a board of n rows and m columns
createEmptyBoard :: Int -> Int -> Board
createEmptyBoard n m = [[0 | j <- [1..m]] | i <- [1..n]]


-- Given a board, determine the number of free cells of it
countFreeCells :: Board -> Int
countFreeCells board = sum $ map (\x -> length $ filter (== 0) x) board


-- Given a column c, Helper function to get the number of pieces consecutives 
-- in a column of the player identified by an integer x in the column c
-- starting from the highest not free cell Column
-- Pre: column c is a valid position
maxpiece_col :: [Int] -> Int -> Int
maxpiece_col l x = length $ takeWhile (== x) (dropWhile (== 0) (reverse l))


-- Given a row represented by a list of integer, return the number of consecutive pieces of player identified by integer x having one of the pieces in the column pos
-- Pre: column pos is a valid position
max_actual_row :: [Int] -> Column -> Int-> Int
max_actual_row board pos x = do
	if pos == 0 then do
		length $ takeWhile (\aux -> aux == x) board
	else do
		if pos == (length board - 1) then do
			length $ takeWhile (\aux -> aux == x) (reverse board)
		else do
			(length $ takeWhile (\aux -> aux == x) (drop pos board)) + (length $ takeWhile (\aux -> aux == x) (reverse $ take pos board))


-- Given a board b, the number of rows, check the number of consecutive pieces of the upper diagonal 
-- started by a position of the matrix represented by a tuple of integers.
act_diagonal_up :: Board -> Int -> (Int,Int) -> Int -> Int
act_diagonal_up board n (x,y) val
	| x == (-1) = 0
	| y == n = 0
	| valueAt board (x,y) == val = 1 + act_diagonal_up board n (x-1, y+1) val
	| otherwise = 0


-- Given a board b, the number of rows, check the number of consecutive pieces of the down diagonal 
-- started by a position of the matrix represented by a tuple of integers.			
act_diagonal_down :: Board -> Int -> (Int,Int) -> Int -> Int
act_diagonal_down board n (x,y) val
	| x == n = 0
	| y == (-1) = 0
	| valueAt board (x,y) == val = 1 + act_diagonal_down board n (x+1, y-1) val
	| otherwise = 0


-- Given a board b, the number of rows, check the number of consecutive pieces of the upper antidiagonal 
-- started by a position of the matrix represented by a tuple of integers.			
act_antidiagonal_up :: Board -> (Int,Int) ->  (Int,Int) -> Int -> Int
act_antidiagonal_up board (n,m) (x,y) val
	| x == n = 0
	| y == m = 0
	| valueAt board (x,y) == val = 1 + act_antidiagonal_up board (n,m) (x+1, y+1) val
	| otherwise = 0


-- Given a board b, the number of rows, check the number of consecutive pieces of the down antidiagonal 
-- started by a position of the matrix represented by a tuple of integers.
act_antidiagonal_down :: Board ->  (Int,Int) -> Int -> Int
act_antidiagonal_down board (x,y) val
	| x == (-1) = 0
	| y == (-1) = 0
	| valueAt board (x,y) == val = 1 + act_antidiagonal_down board (x-1, y-1) val
	| otherwise = 0


-- Given a board, a player p and a column pos, return the maximum consecutive number of pieces of the row, column and the two diagonals 
-- by puting the piece in the column pos.
-- Pre: column pos is a valid position
get_max_row_col_diags :: Board -> Player -> Column -> [Int]
get_max_row_col_diags board player pos = do
	let n = length board
	let m = (length $ head board)
	-- Largest consecutive number of pieces in the column pos
	let max_col = maxpiece_col (board!!pos) (if player == Bot then 2 else 1)
	-- Largest consecutive number of pieces in the highest not free row of the column pos 
	let act_row = (length $ takeWhile (/= 0) (board!!pos)) - 1
	let max_row = max_actual_row ((change_to_row board) !! (m - 1 - act_row)) pos (if player == Bot then 2 else 1)
	-- Largest consecutive number of pieces in the two diagonals of the column pos
	-- in both max_diag and max_antidiag, we need to minus 1 because we count the central element twice
	let max_diag = (act_diagonal_up board m (pos,act_row) (if player == Bot then 2 else 1)) + (act_diagonal_down board n (pos,act_row) (if player == Bot then 2 else 1)) - 1
	let max_antidiag = (act_antidiagonal_up board (n,m) (pos,act_row) (if player == Bot then 2 else 1)) + (act_antidiagonal_down board (pos,act_row) (if player == Bot then 2 else 1)) - 1
	[max_col,max_row,max_diag,max_antidiag]

-- Given a board and a player p and a column pos, return whether the player can have 4 connected pieces in the column pos
-- Pre: column pos is a valid position
check_four_connected :: Board -> Player -> Column -> Bool
check_four_connected board player pos = do
	let res_list = get_max_row_col_diags board player pos
	any (>= 4) res_list

-- Given a board , a column c and a player p, return the nubmer of maximum_consecutive pieces it has after playing the column c
-- and the position as a tuple of (int, column)
-- Note that in the function check_four_connected you don't put the piece in the column c 
-- Pre: column pos is a valid position
max_piece_column :: Board  -> Column -> Player -> (Int,Column)
max_piece_column board pos player = do
	let res_list = get_max_row_col_diags (putPiece board player pos) player pos 
	(maximum res_list, pos)


-- Given a board, return the column that makes 4-connected or the maximum number of consecutive pieces of the Bot
-- without letting the human player win after making that move
-- if there are no such column, return (-1,-1)
-- if there are several columns that fulfills the requisite, then return one of them randomly
get_max_piece_board :: Board -> IO (Int, Column)
get_max_piece_board board = do
	let scores = map (\x -> max_piece_column board x Bot) (getValidColumns board)
	-- We only want those scores that either can make 4-connected or will not let the opponent win
	let aux_scores = filter (\x -> (fst x /= 0) && ((fst x >= 4) || (not $ can_win_human (putPiece board Bot (snd x))))) scores
	if length aux_scores == 0 then do
		return ((-1),(-1))
	else do	
		-- We catch the maximum score, if there is more that one, then we choose one of them randomly
		let max_score = maximum $ map (\x -> fst x) aux_scores
		let final_cols = map (\x -> snd x) (filter (\x -> (fst x) == max_score) aux_scores)
		col <- randInt 0 ((length final_cols) - 1)
		return (max_score, final_cols !! col)


-- Helper function: given a board, check the columns that can let human player win the game
winning_positions :: Board -> [Int]
winning_positions board = filter aux_function [0 .. ((length board) - 1)]
	where 
		aux_function x = do
			let aux_val = max_piece_column board x Player1
			if fst aux_val >= 4	then do
				True
			else do
				False


-- Helper function: given a board, check whether human player can win the game after putting his piece
can_win_human :: Board -> Bool
can_win_human board = not $ null $ winning_positions board


-- Given a board, check whether player p has won the game or not
-- if human player wins, return 1
-- if bot player wins, return 2
-- if nobody wins, return -1
winningplayer :: Board -> Player -> Int -> IO Int
winningplayer board player col = do
	let b = check_four_connected board player col
	if b then do
		return (if player == Player1 then 1 else 2)
	else do
		return (-1)


-- ==========================================================================================
-- Auxiliar Functions ##Strategy Section##
-- ==========================================================================================

-- Given a integer between 1 and 3, both inclusive, return the strategy chosen
selectAI :: Int -> Strategy
selectAI 1 = randStrategy
selectAI 2 = greedyStrategy
selectAI 3 = smartStrategy


-- Return the opposite player of the current player
opposite_player :: Player -> Player
opposite_player Player1 = Bot
opposite_player Bot = Player1


-- ==========================================================================================
-- Random Strategy
-- ==========================================================================================

-- Random strategy: it chooses a random column of the board 
randStrategy :: Strategy
randStrategy board = do
	col <- randInt 0 ((length board) - 1)
	if last (board !! col) /= 0 then do
		randStrategy board
	else do
		return col


-- ==========================================================================================
-- Greedy Strategy
-- ==========================================================================================


-- Greedy strategy: it follows the following rules:

-- first we check that with our greedy implementation it found some columns that:
-- either makes 4-connected or makes the largest consecutive line without letting the opponent win

-- if there is not such column, then we choose randomly a possible column that we can play
-- as we cannot win and all the moves we made will let the opponent win

-- if there is a column that can make 4-connected, then it chooses that column, 
-- if there are more than one, then it selects randomly one of them

-- later we check how many columns can the opponent win with the current board
-- if there are more than one, then we choose randomly a column as we will lose
-- else if there is one, then we block that column

-- Finally we choose the largest consecutive line that we can play without letting the opponent win.
greedyStrategy :: Strategy
greedyStrategy board = do
	max_score_col <- get_max_piece_board board
	let cols = getValidColumns board
	let cols_size = length cols
	if fst max_score_col == (-1) then do
		let cols = getValidColumns board
		rand_col <- randInt 0 (cols_size - 1)
		return $ cols !! rand_col
	else do
		if fst max_score_col >= 4 then do
			return $ snd max_score_col
		else do
			-- positions that the human player can win with the current board
			let opponents_winning = winning_positions board
			if length opponents_winning > 1 then do
				rand_col <- randInt 0 (cols_size - 1)
				return $ cols !! rand_col
			else do
				if length opponents_winning == 1 then do
					return $ head opponents_winning
				else do
					return $ snd max_score_col


-- ==========================================================================================
-- Smart Strategy
-- Precondition: The given board, player and columns are always valid
-- ==========================================================================================

-- Smart strategy: it uses the variant of minimax algorithm, negamax
-- as the function returns a score of the board using depth cut-off and alpha beta pruning strategy
-- what we do is try to make a move, and see the score of the human player
-- then we choose the column that makes the human player obtain the least score possible
-- prioritizing the middle columns
smartStrategy :: Strategy
smartStrategy board = do
	let n = length board
	let m = length $ head board
	let beta = div (n * m) 2 
	let alpha = (-1) * beta
	res <- get_minimax board (getValidColumns_v2 board) alpha beta 
	let max_val = minimum $ map (\x -> snd x) res 
	return $ fst $ head $ filter (\x -> snd x == max_val) res


-- Helper function for smart strategy, it tries all the playable columns
-- and computes the score that will obtain after playing that column
-- by seing the score from the human player point of view using negamax strategy.(so we need to choose the worst score) 
-- then we need to obtain the least score possible
-- any move that Bot can make 4-connected are scored -99999    
get_minimax :: Board -> [Int] -> Int -> Int -> IO [(Column, Int)]
get_minimax	board list alpha beta = do
	if null list then do
		return []
	else do
		let x = head list
		let aux_board = putPiece board Bot x
		if check_four_connected aux_board Bot x then do
			return [(x,(-99999))]
		else do
			let depth = 5
			score <- negamax aux_board Player1 0 depth alpha beta
			aux_list <- get_minimax board (tail list) alpha beta
			return ((x,score) : aux_list)


-- Negamax algorithm, given a board b, a player p, the current depth and the maximum depth
-- and the two factors alpha beta, return the score obtained of the current board
-- Precondition: alpha < beta
-- Position’s score:
-- We define a score for any non final position reflecting the outcome of the game for the player to play, 
-- considering that both players play perfectly and try to win as soon as possible or lose as late as possible. 
-- A position has:
-- a positive score if the current player can win. 1 if he wins with his last stone, 2 if he wins with your second last stone and so on
-- a null score if the game will end by a draw game
-- a negative score if the current player lose whatever he plays. -1 if his opponent wins with his last stone, -2 if his opponent wins with his second last stone and so on
negamax :: Board -> Player -> Int ->  Int -> Int -> Int  -> IO Int
negamax board player depth max_depth alpha beta = do
	if depth >= max_depth then do
		return alpha
	else do
		let n = length board
		let m = length $ head board 
		let n_moves = countFreeCells board
		let moves = getValidColumns_v2 board
		-- check for draw game
		if n_moves == 0 then do
			return 0
		else do
			-- number of free cells
			let pb_moves = n*m - n_moves
			if any (\x -> check_four_connected (putPiece board player x) player x) moves then do
				return $ div (n*m + 1 - pb_moves) 2
			else do
				-- upper bound of our score as we cannot win immediately
				let max_val = div (n*m - 1 - pb_moves) 2
				let beta_aux = min beta max_val
				-- prune the exploration if the [alpha;beta] window is empty.
				if alpha >=  beta_aux then do
					return $ beta_aux
				else do
					-- compute the score of all possible next move and keep the best one
					rec_loop board moves depth max_depth alpha beta_aux player


-- Helper function of the negamax algorithm, it reccursively score connect 4 position using negamax variant of alpha-beta 
-- algorithm.
-- and keep the best score at the moment
rec_loop :: Board -> [Int] -> Int -> Int -> Int -> Int -> Player -> IO Int
rec_loop board list depth max_depth alpha beta player = do
	if null list then do
		return alpha
	else do
		let x = head list
		let aux_board = putPiece board player x
		-- explore opponent's score within [-beta;-alpha] windows:
		-- no need to have good precision for score better than beta (opponent's score worse than -beta)
		-- no need to check for score worse than alpha (opponent's score worse better than -alpha)
		aux <- (negamax aux_board (opposite_player player) (depth + 1) max_depth ((-1)*beta) ((-1)*alpha))
		let aux_2 = (-1) * aux
		-- prune the exploration if we find a possible move better than what we were looking for.
		if aux_2 >= beta then do
			return aux_2
		else do
			-- reduce the [alpha;beta] window for next exploration, as we only
			-- need to search for a position that is better than the best so far.
			rec_loop board (tail list) depth max_depth (max aux_2 alpha) beta player


-- ==========================================================================================
-- Auxiliar functions: Game Section
-- ==========================================================================================


-- Function that given a valid board, returns a valid column that user has inputed while playing the game
getColumn :: Board -> IO Int
getColumn board = do
	putStrLn "Introdueix la columna que vols posar la fitxa"
	s <- getLine
	if not $ all (\c -> c >= '0' && c <= '9') s then do
		putStrLn "Valor no valid, ha de ser numeric, torna a introduir"
		getColumn board
	else do
		let col = read s :: Int
		if (col < 0 || col >= (length board)) then do
			putStrLn "Valor invalid, ha de ser un valorentre 0 i la llargada del tauler, torna a introduir"
			getColumn board
		else do
			if last (board !! col) /= 0 then do
				putStrLn "Columna plena, torna a introduir un altre valor"
				getColumn board
			else do
				return col


-- Given a board, human player chooses a valid column, and we return a tuple of the new updated board with
-- that piece and the column for further checkings
makeTurnHuman :: Board -> IO (Board,Int)
makeTurnHuman board = do
	col <- getColumn board
	return (putPiece board Player1 col, col)


-- Given a board, Bot player chooses a valid column using one of the strategies, and we return a tuple of the new updated board with
-- that piece and the column for further checkings
makeTurnBot :: Board -> Strategy -> IO (Board,Int)
makeTurnBot board strategy = do
	col <- strategy board
	return (putPiece board Bot col, col)


-- Function that runs the turn of each player
makeTurns :: Board -> Player -> Strategy -> IO (Board,Int)
makeTurns board Player1 _ = makeTurnHuman board
makeTurns board _ str1 = makeTurnBot board str1


-- This fuction is the one that controls all the turns of the game
-- it stops when:
-- either there is a draw or there is a winner
-- at the end of each turn, it printed the updated board
-- it returns the score of each player in a tuple (Player1, Bot)
-- 1 means it has win, 0 loses
-- if both of the values are 0, that means that there is a draw 
turn :: Board -> Int -> Strategy -> Int ->IO (Int,Int)
turn board player_turn str1 number_turn = do
	let freeCells = countFreeCells board
	-- check whether there is a draw or not
	if freeCells == 0 then do
		return (0,0)
	else do
		if player_turn == 0 then do
			putStrLn "======================================"
			putStrLn ("TORN " ++ (show number_turn) ++" - Torn Player1")
			putStrLn "====================================== \n"
		else do
			putStrLn "======================================"
			putStrLn ("TORN " ++ (show number_turn) ++" - Torn Bot")
			putStrLn "======================================\n"
		(newBoard,col) <- makeTurns board (if player_turn == 0 then Player1 else Bot) str1
		putStrLn $ paintBoard newBoard
		winner <- winningplayer newBoard (if player_turn == 0 then Player1 else Bot) col
		if  winner /= (-1) then do
			if winner == 1 then do
				return (1,0)
			else do
				return (0,1)
		else do
			turn newBoard (1-player_turn) str1 (number_turn + 1)


-- Play function: starts the game round, print whether the start player is human or bot (chosen randomly)
-- and outputs the final result, whether there is a draw or a winner
play :: Board -> Strategy -> IO ()
play board str1 = do
	start_player <- randInt 0 1
	-- let start_player = 1
	putStrLn ("COMENÇA EL JUGADOR " ++ (if start_player == 0 then "Huma!\n" else "Bot!\n"))
	putStrLn "\x1b[34mJUGADOR HUMA: VERMELL        JUGADOR BOT: VERD\x1b[0m" 
	score <- turn board start_player str1 1
	if fst score == snd score
    	then do putStrLn "\x1b[0mAqui hi ha un empat!"
    	else do putStrLn $ "\x1b[0mJugador " ++ ((\(x,y) -> if x > y then "Huma" else "Bot") score) ++ " ha guanyat!"
  	return ()


-- function that is responsible of the initialization of the game
-- it will make the user enter the valid board size, the difficulty of the game and after that
-- it will start the game
initGame = do
	putStrLn "Introdueix el nombre de files i columnes (format: x y)"
	s <- getLine
	if  not $ validSize s then do
		putStrLn "El valor introduit no es correcte: ha de ser numèric , torno al principi"
		initGame
	else do
		let n = takeWhile (\x -> x >= '0' && x <= '9') s
		let m = dropWhile (\x -> x < '0' || x > '9') (drop (length n) s)
		putStrLn ("selecciona la dificultat del joc:\n" ++ "\t1) FACIL\n" ++ "\t2) MODERAT\n" ++ "\t3) DIFICIL")
		s <- getLine
		if not $ isNum s then do
			putStrLn "El valor ha de ser numeric i ha d'estar entre 1 i 3, torno al principi"
			initGame
		else do
			let bot = read s :: Int
			play (createEmptyBoard (read m :: Int)  (read n :: Int)) (selectAI bot)
			return ()


-- ==========================================================================================
-- Main Program
-- ==========================================================================================
main :: IO ()
main = initGame
