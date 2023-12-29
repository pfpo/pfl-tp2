module Stack (Stack(Stk), StkVal(IntTok, BoolTok),
    push, pop, top, 
    emptyStack, stkVal2Str) where

-- Stack data type. Holds a list of StkVal which can be ints or bools
data StkVal = IntTok Int | BoolTok Bool
data Stack = Stk [StkVal]


-- State data type. Holds a list of (String, StkVal) pairs
type State = [(String, StkVal)]

push :: StkVal -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> StkVal
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

emptyStack :: Stack
emptyStack = Stk []

stkVal2Str :: StkVal -> String
stkVal2Str (IntTok i) = show i
stkVal2Str (BoolTok b) = show b

--------------------

emptyState :: State
emptyState = []

-- Store a value in the state. If the variable already exists, replace it. If it doesn't, add it. Should be ordered alphabetically.
store :: String -> StkVal -> State -> State
store str val [] = [(str, val)]
store str val ((str', val'):xs)
    | str == str' = (str, val):xs
    | str < str' = (str, val):(str', val'):xs
    | otherwise = (str', val'): store str val xs

-- Fetch a value from the state. If the variable doesn't exist, return an error.
fetch :: String -> State -> StkVal
fetch str [] = error "Stack.fetch: variable not found"
fetch str ((str', val'):xs)
    | str == str' = val'
    | otherwise = fetch str xs

state2Str :: State -> String
state2Str [] = ""
state2Str ((str, val):xs) = str ++ "=" ++ stkVal2Str val ++ "," ++ state2Str xs
