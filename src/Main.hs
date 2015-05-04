module Main where

import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import           Text.Read

data Resource
  = Coal
  | Cotton
  | Amber
  deriving (Show,Read,Eq,Ord)

data Place =
  Place {_name        :: String
        ,_description :: String
        ,_buyPrice    :: Map Resource Int
        ,_sellPrice   :: Map Resource Int}
  deriving (Show,Read,Eq)

data Ship =
  Ship {_inventory :: Map Resource Int
       ,_balance   :: Int
       ,_location  :: Place}
  deriving (Show,Read,Eq)

data World = World {_places :: [Place], _ship :: Ship}
  deriving (Show,Read,Eq)

findPlace :: String -> [Place] -> Maybe Place
findPlace n ps =
  case filter (\p -> _name p == n) ps of
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

runCommand :: Command -> State World String
runCommand List =
  do ps <- gets _places
     return $
       "Your places are...\n" ++
       concatMap (\p -> _name p ++ "\n") ps

runCommand (Goto n) =
  do ps <- gets _places
     s <- gets _ship
     case findPlace n ps of
                Just p -> put (World ps (s {_location = p}))
                          >> return ("You have moved to " ++ _name p ++ "\n" ++ _description p)
                Nothing -> return "Where's that!?!??!"

runCommand Wallet = do
   s <- gets _ship
   return $ "You have " ++ show (_balance s) ++ " currency units"

runCommand (Buy n r) =
  do ps <- gets _places
     s <- gets _ship
     let place = _location s
         unitPrice =
           Map.lookup r
                      (_sellPrice place)
         price = fmap (* n) unitPrice
         bal = _balance s
     case price of
       Nothing -> return $ show r ++ " is not for sale here"
       Just p ->
         if p > bal
            then return "Too pricy for you!"
            else put (World ps
                            (s {_balance = _balance s - p
                               ,_inventory =
                                  updateInventory r
                                                  n
                                                  (_inventory s)})) >>
                 return "SOLD!"

runCommand (Sell n r) =
  do ps <- gets _places
     s <- gets _ship
     let place = _location s
         unitPrice =
           Map.lookup r
                      (_buyPrice place)
         price = fmap (* n) unitPrice
         onboard =
           fromMaybe 0 $
           Map.lookup r
                      (_inventory s)
     case price of
       Nothing -> return $ show r ++ " is not wanted here"
       Just p ->
         if n > onboard
            then return $ "You don't have that much " ++ show r
            else put (World ps
                            (s {_balance = _balance s + p
                               ,_inventory =
                                  updateInventory r
                                                  (-n)
                                                  (_inventory s)})) >>
                 return "BOUGHT!"

runCommand Market =
  do s <- gets _ship
     return ("Buy: " ++
             show (_buyPrice (_location s)) ++
             "\n" ++
             "Sell: " ++
             show (_sellPrice (_location s)))

parseCommand :: String -> Maybe Command
parseCommand = readMaybe

tryRunCommand :: String -> State World String
tryRunCommand s =
  case parseCommand s of
    Just c -> runCommand c
    Nothing -> return "I'm sorry Dave, I can't do that."

initialMap :: [Place]
initialMap =
  [Place {_name = "Eclipse"
         ,_description = "A place where the pace of life is slow."
         ,_buyPrice =
            Map.fromList [(Cotton,10),(Coal,5)]
         ,_sellPrice =
            Map.fromList [(Amber,43),(Coal,6)]}
  ,Place {_name = "Vim"
         ,_description = "A highly advanced society"
         ,_buyPrice =
            Map.fromList [(Cotton,12),(Amber,50)]
         ,_sellPrice =
            Map.fromList [(Amber,43),(Coal,6)]}]

start :: World
start =
  World initialMap (Ship Map.empty 20 (head initialMap))

run :: World -> IO World
run w = do
  input <- getLine
  let (msg, w') = runState (tryRunCommand input) w
  putStrLn msg
  run w'

main :: IO World
main = do
  putStrLn "Welcome to space"
  run start
