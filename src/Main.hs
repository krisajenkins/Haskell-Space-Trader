module Main where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import           Text.Read

data Resource
  = Coal
  | Cotton
  | Amber
  deriving (Show,Read,Eq,Ord)

data Place =
  Place {name        :: String
        ,description :: String
        ,buyPrice    :: Map Resource Int
        ,sellPrice   :: Map Resource Int}
  deriving (Show,Read,Eq)

data Ship =
  Ship {inventory :: Map Resource Int
       ,balance   :: Int
       ,location  :: Place}
  deriving (Show,Read,Eq)

data World = World [Place] Ship
  deriving (Show,Read,Eq)

findPlace :: String -> [Place] -> Maybe Place
findPlace n ps =
  case filter (\p -> name p == n) ps of
       [p] -> Just p
       _ -> Nothing

data Command
  = List
  | Goto String
  | Buy Int
        Resource
  | Sell Int
         Resource
  | Market
  | Wallet
  deriving (Read)

updateInventory :: Resource -> Int -> Map Resource Int -> Map Resource Int
updateInventory r n i =
  case Map.lookup r i of
    Just v -> Map.insert r (v + n) i
    Nothing -> Map.insert r n i

runCommand :: Command -> World -> (World, String)
runCommand List w@(World ps _) =
  (w,"Your places are...\n" ++ x)
  where x = concatMap (\p -> name p ++ "\n") ps
runCommand (Goto n) w@(World ps s) =
  case findPlace n ps of
    Just p ->
      (World ps (s {location = p})
      ,"You have moved to " ++ name p ++ "\n" ++ description p)
    Nothing -> (w,"Where's that!?!??!")
runCommand Wallet w@(World _ s) =
  (w, "You have " ++ show (balance s) ++ " currency units")
runCommand (Buy n r) w@(World ps s) =
  let place = location s
      unitPrice = Map.lookup r (sellPrice place)
      price = fmap (*n) unitPrice
      bal = balance s
  in case price of
     Just p ->
       if p > bal
          then (w, "Too pricy for you!")
          else (World ps (s { balance = balance s - p
                    , inventory = updateInventory r n (inventory s)
                    }),
        "SOLD!")
     Nothing -> (w, show r ++ " is not for sale here")
runCommand (Sell n r) w@(World ps s) =
  let place = location s
      unitPrice =
        Map.lookup r
                   (buyPrice place)
      price = fmap (* n) unitPrice
      onboard =
        fromMaybe 0 $
        Map.lookup r
                   (inventory s)
  in case price of
       Just p ->
         if n > onboard
            then (w,"You don't have that much " ++ show r)
            else (World ps
                        (s {balance = balance s + p
                           ,inventory =
                              updateInventory r
                                              (-n)
                                              (inventory s)})
                 ,"BOUGHT!")
       Nothing ->
         (w,show r ++ " is not wanted here")
runCommand Market w@(World _ s) =
  (w
  ,"Buy: " ++
   show (buyPrice (location s)) ++
   "\n" ++
   "Sell: " ++
   show (sellPrice (location s)))

parseCommand :: String -> Maybe Command
parseCommand = readMaybe

tryRunCommand :: String -> World -> (World, String)
tryRunCommand s w =
  case parseCommand s of
    Just c -> runCommand c w
    Nothing -> (w, "I'm sorry Dave, I can't do that.")

initialMap :: [Place]
initialMap =
  [Place {name = "Eclipse"
         ,description = "A place where the pace of life is slow."
         ,buyPrice =
            Map.fromList [(Cotton,10),(Coal,5)]
         ,sellPrice =
            Map.fromList [(Amber,43),(Coal,6)]}
  ,Place {name = "Vim"
         ,description = "A highly advanced society"
         ,buyPrice =
            Map.fromList [(Cotton,12),(Amber,50)]
         ,sellPrice =
            Map.fromList [(Amber,43),(Coal,6)]}]

start :: World
start =
  World initialMap (Ship Map.empty 20 (head initialMap))

run :: World -> IO World
run w = do
  input <- getLine
  let (w', msg) = tryRunCommand input w
  putStrLn msg
  run w'

main :: IO World
main = do
  putStrLn "Welcome to space"
  run start
