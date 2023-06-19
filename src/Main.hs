--FLP 2022/2023 – funkcionální projekt: Haskell - Knapsack problem, xbilja00, Jan Bíl, 2023

import System.Environment
import Control.Monad
import Data.List
import Data.Char
import Data.Ord
import KnapsackModule
import Debug.Trace
import System.Random

--Gets arguments, while -i, -b or -o options are there, it remembers it and returns it with rest of arguments (input file or [])
parseArgs :: [String] -> [Bool] -> ([Bool], [String])
parseArgs ("-i" : xs) (_:y:z) = parseArgs xs (True : y : z)
parseArgs ("-b" : xs) (x:_:z) = parseArgs xs (x : True : z)
parseArgs ("-o" : xs) (x:y:_) = parseArgs xs (x : y : True :[])
parseArgs [] (x:y:z) = ((x:y:z), [])
parseArgs a (x:y:z) = ((x:y:z), a)
parseArgs _ _ = error "Invalid arguments"

--Is input file given
isInputFile :: [String] -> Bool
isInputFile (_:_) = True
isInputFile [] = False

getInputFromFile :: [Char] -> IO [Char]
getInputFromFile inputFile = readFile inputFile

getInput :: [[Char]] -> IO [Char]
getInput x = if isInputFile x then getInputFromFile (head x) else getInputWOFile

--Gets given Knapsack from stdin
getInputWOFile :: IO [Char]
getInputWOFile = getContents

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Not a right form of maxWeight or minCost"

--Get max weight
parsemaxW :: String -> Int
parsemaxW str = if isPrefixOf "maxWeight: " str 
                    then let a = stripPrefix "maxWeight: " str in 
                        read (fromJust a) :: Int
                else trace str (-1)

--Get min cost
parseminC :: String -> Int
parseminC str = if isPrefixOf "minCost: " str 
                    then let a = stripPrefix "minCost: " str in 
                        read (fromJust a) :: Int
                    else trace str (-1)

--Get weight of item
parseWeight :: String -> Int
parseWeight str = if isInfixOf "weight: " str 
                    then read (filter (\x -> isDigit x) str) :: Int
                else trace str (-1)

--Get cost of item
parseCost :: String -> Int
parseCost str = if isInfixOf "cost: " str 
                    then read (filter (\x -> isDigit x) str) :: Int
                else trace str (-1)

--Parse next item
parseItem :: [String] -> Item
parseItem (_ : cweight : ccost : _) = Item {
    weight = parsedWeight
    ,cost = parsedCost
    }
    where parsedWeight = parseWeight cweight
          parsedCost = parseCost ccost
parseItem _ = (error "Invalid input!")

--Remove one item from list of strings so another can be parsed
removeItem :: [String] -> [String]
removeItem (_ : _ : _ : _ : x) = x
removeItem _ = (error "Invalid input!")

--Parse all items
parseItems :: [String] -> [Item]
parseItems ("]" : _) = []
parseItems x@(_ : _ : _ : _) =  (parseItem x : parseItems (removeItem x))
parseItems (_ : _) = []
parseItems _ =  error "Invalid input!"


--Converts set of words (actually lines) to my representation of BKG
parse :: [String] -> Knapsack
parse (_ : maxW : minC : _ : citems) = Knapsack {
    maxWeight = parsedmaxW
    ,minCost = parsedminC
    ,items = parsedItems
  }
  where parsedmaxW = parsemaxW maxW
        parsedminC = parseminC minC
        parsedItems = parseItems citems
parse _ =  error "Invalid input!"

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

--For given list of items and index i return item on position i
getItembyIndex :: [Item] -> Int -> Item
getItembyIndex [] _ = error "Index out of range"
getItembyIndex (x:_) 1 = x
getItembyIndex (_:xn) n = getItembyIndex xn (n-1)

--Returns weight and cost in a list (tuple possible)
getParams :: Item -> [Int]
getParams item = ([(weight item)] ++ [(cost item)])

--Sum weight of items in list of indexes.
weightOfOption :: [Item] -> [Int] -> Int
weightOfOption _ [] = 0
weightOfOption citems (x:xs) = head (getParams (getItembyIndex citems x)) + weightOfOption citems xs

--Sum cost of items in list of indexes.
costOfOption :: [Item] -> [Int] -> Int
costOfOption _ [] = 0
costOfOption citems (x:xs) = head (tail(getParams (getItembyIndex citems x))) + costOfOption citems xs

--Calculates wheather one option is better then the other one
isItBetterOption :: Knapsack -> [Int] -> [Int] -> Bool
isItBetterOption knapsack x y = if costOfOption (items knapsack) x > costOfOption (items knapsack) y &&
     weightOfOption (items knapsack) x <= (maxWeight knapsack) && costOfOption (items knapsack) x >= (minCost knapsack) then True      
                       else False

--For list of option returns the best one.
findBestOption :: Knapsack -> [[Int]] -> [Int] -> [Int]
findBestOption _ [] y = y
findBestOption knapsack (x:xs) y = if isItBetterOption knapsack x y then findBestOption knapsack xs x
                                    else findBestOption knapsack xs y

-- Creates all subsets of input list of numbers.
makeSubsets :: [Int] -> Int -> [Int] -> [[Int]]
makeSubsets citems index cin = if index == (length citems) 
                                then (cin : [(index : cin)]) 
                                else (makeSubsets citems (index+1) cin) ++ (makeSubsets citems (index+1) (index:cin))

--set bit at given index to 1
setBit :: Int -> [Int] -> [Int]
setBit _ [] = []
setBit 1 (_:xs) = (1:xs)
setBit n (x:xs) = (x: (setBit (n-1) xs))

-- Set multiple bits.
setOutput :: [Int] -> [Int] -> [Int]
setOutput [] y = y 
setOutput (x:xs) y = setOutput xs (setBit x y)

--Returns best solution for given knapsack problem.
bruteforce :: Knapsack -> [Int]
bruteforce knapsack = if isEmpty (items knapsack) then [] else (findBestOption knapsack (makeSubsets [1..(length (items knapsack))] 1 []) [])

--Function to output solution.
checkBruteforce :: Knapsack -> IO()
checkBruteforce knapsack = if snd (head (addFitness knapsack [a])) < (minCost knapsack) then putStrLn "False" else putStrLn ("Solution " ++ show a) where a = setOutput (bruteforce knapsack) (replicate (length (items knapsack)) 0)

--Converts integer to binary in form of list.
binarz :: Int -> [Int]
binarz 0 = []
binarz n = binarz (div n 2) ++ [(mod n 2)] 

--Converts integer to list of binary values. Instead of [] returns [0]
toBinary :: Int -> [Int]
toBinary n = let a = binarz n in if a == [] then [0] else a

--Adds zero bits at beggining unless it is long enough
addZeroBits :: [Int] -> Int -> [Int]
addZeroBits list n = if length list < n then addZeroBits (0:list) n else list

--Calculates cost of items in a candidate solution
sumCost :: [Item] -> [Int] -> Int
sumCost _ [] = 0
sumCost [] _ = 0
sumCost (x:xs) (y:ys) = if y == 1 then (cost x) + (sumCost xs ys) else sumCost xs ys

--Calculates weight of items in a candidate solution
sumWeight :: [Item] -> [Int] -> Int
sumWeight _ [] = 0
sumWeight [] _ = 0
sumWeight (x:xs) (y:ys) = if y == 1 then (weight x) + (sumWeight xs ys) else sumWeight xs ys

--For candidate solution calculates fitness function, which is sum of costs of items if weight of items is smaller then maximum weight. 0 otherwise.
fitnessFunc :: Knapsack -> [Int] -> Int
fitnessFunc knapsack state = let ff = sumCost (items knapsack) state in if sumWeight (items knapsack) state <= (maxWeight knapsack) then ff else 0

--For set of candidate solutions, add fitness functions.
addFitness :: Knapsack -> [[Int]] -> [([Int], Int)]
addFitness _ [] = []
addFitness knapsack (x:xs) = (x, (fitnessFunc knapsack x)) : addFitness knapsack xs 

--Takes number of items and number n and generates n random candidate solutions. It just generates number from 0 to 2^(number of items)-1 and converts it to binary form.
generateRandomStates :: StdGen -> Int -> Int -> [[Int]]
generateRandomStates _ 0 _ = []
generateRandomStates gen n numItems = let a = (randomR (0,((2^numItems)-1)) gen :: (Int, StdGen)) in ((addZeroBits (toBinary (fst a)) numItems) : generateRandomStates (snd a) (n-1) numItems)

--Takes list of solutions and its fitness function value and sums these values.
--sumFitness :: [([Int], Int)] -> Int
--sumFitness [] = 0
--sumFitness (x:xs) = snd x + (sumFitness xs)

--Takes solutions and its fitness funtion values and calculats cumulative distribution function. For example:
-- takes list with these values: [40,20,30,10] and returns [0.4, 0.6, 0.9, 1]
--normalizeFitness :: [([Int], Int)] -> Int -> Double -> [([Int], Double)]
--normalizeFitness [] _ _ = []
--normalizeFitness (x:xs) sum cumulative = if snd x == 0 then (fst x, cumulative): normalizeFitness xs sum cumulative else (fst x, cumulative + ((fromIntegral (snd x)/(fromIntegral sum)))) : normalizeFitness xs sum (cumulative + ((fromIntegral (snd x)/(fromIntegral sum))))

--Takes two states. One with better fitness function is returned.
duel :: (([Int], Int), ([Int], Int)) -> ([Int], Int)
duel (a, b) = if snd a >= snd b then a else b

--Returns state with given index.
pickToDuel :: [([Int], Int)] -> Int -> ([Int], Int)
pickToDuel [] _ = ([], 0) 
pickToDuel (x:_) 0 = x
pickToDuel (_:xs) n = pickToDuel xs (n-1)

--Returns tuple with two random different numbers from zero to n.
generateRandomTuple :: StdGen -> Int -> ((Int, Int), StdGen)
generateRandomTuple gen n = if fst rnd == fst rnd2 then generateRandomTuple (snd rnd2) n else ((fst rnd, fst rnd2), snd rnd2) where 
    rnd = (randomR (0,n) gen :: (Int, StdGen))
    rnd2 = (randomR (0,n) (snd rnd) :: (Int, StdGen))

--Generates two rundom numbers, which represent indexes of chosen states which are picked to tournament. Winner is returned.
generateDuel :: StdGen -> [([Int], Int)] -> (([Int], Int), StdGen)
generateDuel gen list = (duel (pickToDuel list (fst (fst tuple)), pickToDuel list (snd(fst tuple))), snd tuple) where
    tuple = generateRandomTuple gen ((length list)-1)


--Takes two solutions and a number which represents position at which are solutions cut and exchanged. It correspondes to crossover funtion.
crossoverFunc :: ([Int], [Int]) -> Int -> ([Int], [Int])
crossoverFunc (_, []) _ = ([], [])
crossoverFunc ([], _) _ = ([], [])
crossoverFunc ((xs), (ys)) 0 = (ys, xs)
crossoverFunc ((x:xs), (y:ys)) n = let a = crossoverFunc (xs, ys) (n-1) in (x:(fst a), y:(snd a))

--Makes tournament to determine new generation and applies crossover function
makeNewGene :: StdGen -> [([Int], Int)] -> Int-> ([[Int]], StdGen)
makeNewGene gen _ 0 = ([], gen)
makeNewGene gen list n = ((fst parents):(snd parents): (fst newGene), (snd newGene)) where 
    winner = (generateDuel gen list)
    winner2 = generateDuel (snd winner) list
    rnd2 = (randomR (0,(length list)-1) (snd winner2) :: (Int, StdGen))
    parents = crossoverFunc (fst (fst winner), fst (fst winner2)) (fst rnd2)
    newGene = makeNewGene (snd rnd2) list (n-1)


--Changes one bit in a list at given position
changeBit :: [Int] -> Int -> [Int]
changeBit [] _ = []
changeBit (x:xs) 0 = if x == 0 then (1:xs) else (0:xs)
changeBit (x:xs) n = x:(changeBit xs (n-1))

--Generates random position in a list and changes bit at its position. That simulates mutation.
mutate :: StdGen -> [Int] -> ([Int], StdGen)
mutate gen list = (changeBit list (fst rnd), (snd rnd)) where
    rnd = (randomR (0,(length list)-1) gen :: (Int, StdGen))


--Randomly pickes which solutions in current generation are mutated (one random bit in it is changed). Probability is hardcoded in if.
mutateGeneration :: StdGen -> [[Int]] -> ([[Int]], StdGen)
mutateGeneration gen [] = ([], gen)
mutateGeneration gen (x:xs) = if fst rnd < 0.25 then ((fst mutated):(fst mutatedGeneration), snd mutatedGeneration) else (x:(fst mutatedGeneration), snd mutatedGeneration) where
    rnd = (randomR (0,1) gen :: (Double, StdGen))
    mutated = mutate (snd rnd) x
    mutatedGeneration = mutateGeneration (snd mutated) xs

--Simulates evolution of n new generations with application of mutation. 
makeNGenerations :: StdGen -> Knapsack -> Int -> [[Int]] -> [[Int]]
makeNGenerations gen knapsack 0 states =  fst (makeNewGene gen rstates 25) where 
    rstates = addFitness knapsack states
makeNGenerations gen knapsack n states =(makeNGenerations (snd newStates) knapsack (n-1) (fst newStates)) where 
    rstates = addFitness knapsack states
    newGen = (makeNewGene gen rstates 25)
    newStates = mutateGeneration (snd newGen) (fst newGen)

--Gives best solution from current population
findBest :: [([Int], Int)] -> [Int]
findBest list = fst (maximumBy (comparing snd) list)

--Calls evolution from randomly generated states and pick best solution from last generation.
geneticAlgorithm :: StdGen -> StdGen -> Knapsack -> [Int]
geneticAlgorithm gen gen' knapsack = let states = generateRandomStates gen 50 (length (items knapsack)) in findBest(addFitness knapsack (makeNGenerations gen' knapsack 500 states))

--Function to output optimalization algo.
checkGenetic :: Knapsack -> StdGen -> StdGen -> String
checkGenetic knapsack gen gen' = if snd (head (addFitness knapsack [a])) < (minCost knapsack) then "False" else "Solution " ++ (show a) where a = geneticAlgorithm gen gen' knapsack

main :: IO()
main = do
    args <- getArgs
    gen <- getStdGen
    gen' <- newStdGen
    let myArgs = parseArgs args (False:False:False:[])
    contents <- getInput (snd myArgs)
    let knapsack = parse $ lines contents
    when (head (fst myArgs) == True) $ print knapsack
    when (head (tail (fst myArgs)) == True) $ checkBruteforce knapsack
    when (head (tail (tail (fst myArgs))) == True) $ putStrLn (checkGenetic knapsack gen gen')
