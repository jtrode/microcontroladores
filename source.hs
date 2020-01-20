import Text.Show.Functions
import Data.List

data Micro = Micro {
    memory :: [Int],
    a :: Int,
    b :: Int,
    pc :: Int,
    errorMsg :: String,
    program :: [Instruction]
} deriving (Show)

type Instruction = Micro -> Micro
  
addCounter micro = micro {
  pc = pc micro + 1
}

setA valor micro = micro {
  a = valor
}

setB valor micro = micro {
  b = valor
}

setMemo list micro = micro {
  memory = list
}

nop = addCounter

add micro = addCounter.setB 0.setA (a micro + b micro) $ micro
  
swap micro = addCounter.setB (a micro).setA (b micro) $ micro

lodv valor = addCounter.setA valor

divide (Micro m a 0 pc e p) = addCounter (Micro m a 0 pc "DIVISION BY ZERO" p ) 
divide micro = addCounter.setB 0.(setA (a micro `div` b micro)) $ micro
  
lod addr micro 
  | emptyMemo micro = micro
  | otherwise = addCounter.setA (memory micro !! (addr - 1)) $ micro

str addr val micro 
  | emptyMemo micro = addCounter.setMemo ((take (addr - 1) [0,0..]) ++ [val]) $ micro
  | addr <= ((length.memory) micro) = 
    addCounter.setMemo ((take (addr - 1) (memory micro)) ++ [val] ++ (drop (addr) (memory micro))) $ micro
  | otherwise = 
    addCounter.setMemo ((memory micro) ++ (take (addr - ((length.memory) micro) - 1) [0,0..]) ++ [val]) $micro

emptyMemo = (==[]).memory

strProgram program micro = micro {
  program = program
}

run (Micro m a b pc "" p) [] = (Micro m a b pc "" p)
run (Micro m a b pc "" p) (x:xs) = run (x (Micro m a b pc "" p)) xs
run micro _ = micro

runProgram (Micro m a b pc e []) = (Micro m a b pc e [])
runProgram micro = run ((head (program micro)) micro) (tail (program micro))

ifnz micro [] = micro
ifnz (Micro m 0 b pc e p) _ = (Micro m 0 b pc e p)
ifnz (Micro m a b pc "" p) (x:xs) = ifnz (x (Micro m a b pc "" p)) xs
ifnz micro _ = micro

debug = filter (not.aBug)

aBug f = (==0).sum.(++ [a.f $ xt8088]).(++ [b.f $ xt8088]).memory.f $ xt8088

orderedMemo (Micro [] _ _ _ _ _) = True
orderedMemo (Micro (x:xs) _ _ _ _ _) = (all (>= x) xs) && orderedMemo (Micro xs 0 0 0 "" [])

xt8088= Micro [] 0 0 0 "" []

fp20 = Micro [] 7 24 0 "" []

at8086 = Micro [1..20] 0 0 0 "" []

mi8088 = Micro [0,0..] 0 0 0 "" []

add22to10 = [lodv 10, swap, lodv 22, add]

divide2by0 = [str 1 2, str 2 0, lod 2, swap, lod 1, divide]

addCounter :: Instruction
setA :: Int -> Instruction
setB :: Int -> Instruction
setMemo :: [Int] -> Instruction
nop :: Instruction
add :: Instruction
swap :: Instruction   
lodv :: Int -> Instruction 
divide :: Instruction
lod :: Int -> Instruction  
str :: Int -> Int -> Instruction
emptyMemo :: Micro -> Bool
strProgram :: [Instruction] -> Micro  -> Micro
run :: Micro -> [Instruction] -> Micro
runProgram :: Instruction
ifnz :: Micro -> [Instruction] -> Micro
debug :: [Instruction] -> [Instruction]
aBug :: Instruction -> Bool
orderedMemo :: Micro -> Bool
