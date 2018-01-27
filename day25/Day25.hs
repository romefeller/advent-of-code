module Day25 where

data Bin = Zero | One deriving (Enum, Eq, Read, Show)

data Alphabet = A | B | C | D | E | F deriving (Read,Show)

newtype State s a = State {runState :: s -> (s,a)} 

instance Functor (State s) where 
    fmap f (State ssa) = State $ \s -> fmap f (ssa s)

-- f :: s -> (s,a->b)
instance Applicative (State s) where 
    pure a = State $ \s -> (s,a)
    State ssab <*> State ssa = State $ \s -> let (s',ab) = ssab s 
                                                 (s'',a) = ssa s' 
                                             in  (s'', ab a)  

instance Monad (State s) where 
    return a = State $ \s -> (s,a)
    (State ssa) >>= assb = State $ \s -> let (s',a) = ssa s 
                                             State ssb = assb a 
                                         in  ssb s'
    
data Zipper a = Zipper {zl :: [a], zm :: a, zr :: [a]} deriving (Show, Eq)

instance Functor Zipper where 
    fmap f (Zipper l m r) = Zipper (fmap f l) (f m) (fmap f r)


---- Mimic the infinite tape ---------
goLeft :: Zipper Bin -> Zipper Bin
goLeft (Zipper [] m bs) = Zipper [] Zero (m : bs)
goLeft (Zipper (a : as) m bs) = Zipper as a (m : bs)

goRight :: Zipper Bin -> Zipper Bin
goRight (Zipper as m []) = Zipper (m : as) Zero []
goRight (Zipper as m (b : bs)) = Zipper (m : as) b bs
--------------------------------------

zeros :: Zipper Bin
zeros = Zipper [] Zero [] 

writeTape :: Bin -> Zipper Bin -> Zipper Bin
writeTape b (Zipper l _ r) = Zipper l b r  

change1 :: Alphabet -> State (Zipper Bin) Alphabet
change1 A = State $ \z -> (if zm z == Zero then (goRight $ writeTape One z) else (goLeft $ writeTape Zero z),B) 
change1 B = State $ \z -> (if zm z == Zero then (goLeft $ writeTape One z) else (goRight $ writeTape One z),A) 

change :: Alphabet -> State (Zipper Bin) Alphabet
change A = State $ \z -> (if zm z == Zero then (goRight $ writeTape One z) else (goLeft $ writeTape Zero z),B) 
change B = State $ \z -> 
    if zm z == Zero then 
        (goLeft $ writeTape One z,C) 
    else
        (goRight $ writeTape Zero z,E) 
change C = State $ \z -> 
    if zm z == Zero then 
        (goRight $ writeTape One z,E) 
    else
        (goLeft $ writeTape Zero z,D) 
change D = State $ \z -> (goLeft $ writeTape One z,A) 
change E = State $ \z -> 
    if zm z == Zero then 
        (goRight $ writeTape Zero z,A) 
    else
        (goRight $ writeTape Zero z,F) 
change F = State $ \z -> 
    if zm z == Zero then 
        (goRight $ writeTape One z,E) 
    else
        (goRight $ writeTape One z,A) 

program :: Int -> Alphabet -> (Alphabet -> State (Zipper Bin) Alphabet) -> State (Zipper Bin) Alphabet 
program n al config
    | n <= 0 = return al
    | otherwise = config =<< program (n-1) al config

solve :: Int -> Zipper Bin -> Alphabet -> (Int, (Zipper Bin,Alphabet))
solve n iz al = let z = (runState $ program n al change) iz
          in ((fromEnum $ zm $ fst z) + (sum $ map fromEnum $ zl $ fst z) + (sum $ map fromEnum $ zr $ fst z)
             , z)
             