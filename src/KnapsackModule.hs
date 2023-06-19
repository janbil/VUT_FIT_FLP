--FLP 2022/2023 – funkcionální projekt: Haskell - Knapsack problem, xbilja00, Jan Bíl, 2023

module KnapsackModule
(   Weight,
    Cost,
    MaxWeight,
    MinCost,
    Item(..),
    Knapsack(..)) where
type Weight = Int
type Cost = Int
type MaxWeight = Int
type MinCost = Int
data Item = Item {
    weight :: Weight
    ,cost :: Cost
}
data Knapsack = Knapsack {
    maxWeight :: MaxWeight
    ,minCost :: MinCost
    ,items :: [Item]
}

indent :: String -> String
indent = unlines . map ('\t' :) . lines

instance Show Item where
  show  Item {weight = w,cost =c}  = "Item {\nweight: " ++ show w ++ "\ncost: " ++ show c ++ "\n}"
myconcat :: [Item] -> String
myconcat [] = []
myconcat x = "[\n" ++ indent (con x) ++ "]" where
    con [] = []
    con (xx:xs) = show xx ++ "\n" ++ con xs
instance Show Knapsack where
  show  Knapsack {maxWeight = w,minCost =c, items = []}  = "Knapsack {\nmaxWeight: " ++ show w ++ "\nminCost: " ++ show c ++ "\n}"
  show  Knapsack {maxWeight = w,minCost =c, items = x}  = "Knapsack {\nmaxWeight: " ++ show w ++ "\nminCost: " ++ show c ++ "\nitems: " ++ myconcat x ++ "\n}"
