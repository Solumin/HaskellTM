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
    print newTape
    if newS == halt then return newTape
        else runDebug' d newS newTape

delta :: DeltaFunc
delta ("S", Start) = (Keep, MoveRight, "S")
delta ("S", Blank) = (Keep, MoveLeft, "Q")
delta ("S", _) = (Keep, MoveRight, "S")

delta ("Q", Start) = (Keep, Stay, halt)
delta ("Q", Zero) = (Erase, MoveRight, "Q0")
delta ("Q", One) = (Erase, MoveRight, "Q1")

delta ("Q0", Blank) = (PrintZero, MoveLeft, "S")

delta ("Q1", Blank) = (PrintOne, MoveLeft, "S")
