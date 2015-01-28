import Data.List

type State = String
data TapeAction = Erase | Keep | PrintZero | PrintOne deriving (Show, Eq)
data HeadAction = MoveLeft | Stay | MoveRight deriving (Show, Eq)

data Symbol = Start | Blank | Zero | One deriving (Eq)
instance Show Symbol where
    show Start = ">"
    show Blank = "_"
    show Zero = "0"
    show One = "1"

toChar :: Symbol -> Char
toChar Start = '>'
toChar Blank = '_'
toChar Zero = '0'
toChar One = '1'

data Tape = Tape { left :: [Symbol], current :: Symbol, right :: [Symbol] } deriving Eq
instance Show Tape where
   show (Tape ls c rs) = map toChar ls ++ ['[', toChar c, ']'] ++ map toChar rs

halt :: State
halt = "HALT"

blankTape :: Tape
blankTape = Tape [] Start []

newTape :: [Symbol] -> Tape
newTape = Tape [] Start

newIntTape :: [Int] -> Tape
newIntTape nums = Tape [] Start (map (\i -> case i of
    0 -> Zero
    1 -> One) nums)

moveLeft :: Tape -> Tape
moveLeft (Tape _ Start _) = error "Tape cannot move left of the leftmost edge!"
moveLeft (Tape ls c rs) = Tape (init ls) (last ls) (c:rs)

moveRight :: Tape -> Tape
moveRight (Tape ls c []) = Tape (ls ++ [c]) Blank []
moveRight (Tape ls c (r:rs)) = Tape (ls ++ [c]) r rs

stay :: Tape -> Tape
stay = id

doHead :: HeadAction -> Tape -> Tape
doHead MoveLeft = moveLeft
doHead Stay = stay
doHead MoveRight = moveRight

erase :: Tape -> Tape
erase (Tape ls c rs) = Tape ls Blank rs

keep :: Tape -> Tape
keep = id

printTape :: Tape -> Symbol -> Tape
printTape (Tape _ Start _) _ = error "You aren't allowed to touch the Start symbol"
printTape t Start = error "You really shouldn't print a second Start symbol."
printTape (Tape ls c rs) n = Tape ls n rs

printZero :: Tape -> Tape
printZero t = printTape t Zero

printOne :: Tape -> Tape
printOne t = printTape t One

doTape :: TapeAction -> Tape -> Tape
doTape Erase = erase
doTape Keep = keep
doTape PrintZero = printZero
doTape PrintOne = printOne

-- updateTape transforms the tape based on the TapeAction and HeadAction given
updateTape :: Tape -> TapeAction -> HeadAction -> Tape
updateTape tape tact hact = doHead hact (doTape tact tape)

type DeltaFunc = (State, Symbol) -> (TapeAction, HeadAction, State)

runTM :: DeltaFunc -> Tape -> Tape
runTM d = runTM' d "S"

runTM' :: DeltaFunc -> State -> Tape -> Tape
runTM' d state tape = let
    (tact, hact, newS) = d (state, current tape)
    newTape = updateTape tape tact hact in
    if newS == halt then newTape
    else runTM' d newS newTape

runDebug :: DeltaFunc -> Tape -> IO Tape
runDebug d t = do
    print t
    runDebug' d "S" t

runDebug' :: DeltaFunc -> State -> Tape -> IO Tape
runDebug' d state tape = do
    (tact, hact, newS) <- return (d (state, current tape))
    let newTape = updateTape tape tact hact
    putStrLn (show newTape ++ "  " ++ newS)
    if newS == halt then return newTape
        else runDebug' d newS newTape

shiftRight :: DeltaFunc
shiftRight ("S", Start) = (Keep, MoveRight, "S")
shiftRight ("S", Blank) = (Keep, MoveLeft, "Q")
shiftRight ("S", _) = (Keep, MoveRight, "S")

shiftRight ("Q", Start) = (Keep, Stay, halt)
shiftRight ("Q", Zero) = (Erase, MoveRight, "Q0")
shiftRight ("Q", One) = (Erase, MoveRight, "Q1")

shiftRight ("Q0", Blank) = (PrintZero, MoveLeft, "S")

shiftRight ("Q1", Blank) = (PrintOne, MoveLeft, "S")

shiftLeft :: DeltaFunc
shiftLeft ("S", Start) = (Keep, MoveRight, "S")
shiftLeft ("S", Blank) = (Keep, MoveRight, "Q")
shiftLeft ("S", _) = (Keep, MoveLeft, halt)

shiftLeft ("Q", Blank) = (Keep, MoveRight, "Q")
shiftLeft ("Q", Zero) = (Erase, MoveLeft, "Q0")
shiftLeft ("Q", One) = (Erase, MoveLeft, "Q1")

shiftLeft ("Q0", Blank) = (PrintZero, MoveRight, "C")

shiftLeft ("Q1", Blank) = (PrintOne, MoveRight, "C")

shiftLeft ("C", Blank) = (Keep, MoveRight, "C'")

shiftLeft ("C'", Blank) = (Keep, MoveLeft, "D")
shiftLeft ("C'", _) = (Keep, Stay, "Q")

shiftLeft ("D", Start) = (Keep, MoveRight, "H")
shiftLeft ("D", _) = (Keep, MoveLeft, "D")

shiftLeft ("H", Blank) = (Keep, Stay, "Q")
shiftLeft ("H", _) = (Keep, MoveLeft, halt)

binAdd :: DeltaFunc
-- "S" is the start state. It finds the first bit of the first argument and
-- proceeds to the corresponding F state
binAdd ("S", Zero) = (Erase, MoveRight, "F0")
binAdd ("S", One) = (Erase, MoveRight, "F1")
binAdd ("S", _) = (Keep, MoveRight, "S")

-- "F0" indicates that we're adding a 0 to the second argument. It seeks to the
-- end of the first arg.
binAdd ("F0", Blank) = (Keep, MoveRight, "R0")
binAdd ("F0", _) = (Keep, MoveRight, "F0")

-- "F1" is the same as F0, except it indicates addition of 1, not 0.
binAdd ("F1", Blank) = (Keep, MoveRight, "R1")
binAdd ("F1", _) = (Keep, MoveRight, "F1")

binAdd ("R0", Blank) = (Keep, MoveRight, "R0")
binAdd ("R0", Zero) = (Erase, MoveRight, "P0")
binAdd ("R0", One) = (Erase, MoveRight, "P1")

binAdd ("R1", Blank) = (Keep, MoveRight, "R1")
binAdd ("R1", Zero) = (Erase, MoveRight, "P1")
binAdd ("R1", One) = (Erase, MoveRight, "PC")

binAdd ("P0", Blank) = (Keep, MoveRight, "W0")
binAdd ("P0", _) = (Keep, MoveRight, "P0")

binAdd ("P1", Blank) = (Keep, MoveRight, "W1")
binAdd ("P1", _) = (Keep, MoveRight, "P1")

binAdd ("PC", Blank) = (Keep, MoveRight, "WC")
binAdd ("PC", _) = (Keep, MoveRight, "PC")

-- TODO: W0 and W1 should check for a carry bit
-- Move right 2
-- If 1: Erase, move left 2
--  W0: keep, stay, W1
--  W1: keep, stay, WC
binAdd ("W0", Blank) = (PrintZero, MoveLeft, "B")
binAdd ("W0", _) = (Keep, MoveRight, "W0")

binAdd ("W1", Blank) = (PrintOne, MoveLeft, "B")
binAdd ("W1", _) = (Keep, MoveRight, "W1")

binAdd ("WC", Blank) = (PrintZero, MoveRight, "WCC")
binAdd ("WC", _) = (Keep, MoveRight, "WC")

binAdd ("WCC", Blank) = (Keep, MoveRight, "WCC'")

binAdd ("WCC'", Blank) = (PrintOne, MoveLeft, "WCB")

binAdd ("WCB", _) = (Keep, MoveLeft, "B")

-- "B" and "B1" return the head to the start of the tape.
binAdd ("B", Blank) = (Keep, MoveLeft, "B1")
binAdd ("B", _) = (Keep, MoveLeft, "B")

binAdd ("B1", Start) = (Keep, Stay, halt)
binAdd ("B1", Blank) = (Keep, MoveLeft, "B1")
binAdd ("B1", _) = (Keep, MoveLeft, "C")

-- "C" is the "Continue" state. If we're in C when we find the Start symbol, we
-- still have more bits to process
binAdd ("C", Start) = (Keep, Stay, "S")
binAdd ("C", _) = (Keep, MoveLeft, "C")

binAdd m = error $ "Unknown m-config " ++ show m
