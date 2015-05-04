{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Lens
import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (fromMaybe)
import           Text.Read           hiding (get)

data Resource
  = Coal
  | Cotton
  | Amber
  deriving (Show,Read,Eq,Ord)

data Place =
  Place {_name        :: String
        ,_description :: String
        ,_buyPrices   :: Map Resource Int
        ,_sellPrices  :: Map Resource Int}
  deriving (Show,Read,Eq)

data Ship =
  Ship {_inventory :: Map Resource Int
       ,_balance   :: Int
       ,_location  :: Place}
  deriving (Show,Read,Eq)

data World = World {_places :: [Place], _ship :: Ship}
  deriving (Show,Read,Eq)

makeLenses ''Place
makeLenses ''Ship
makeLenses ''World

here :: Lens' World Place
here = ship . location

findPlace :: String -> [Place] -> Maybe Place
findPlace n ps =
  case filter (\p -> (view name) p == n) ps of
       [p] -> Just p
       _ -> Nothing

data Command
  = List
  | Goto String
  | Buy Int Resource
  | Sell Int Resource
  | Market
  | Wallet
  deriving (Read)

trade :: Resource -> Int -> Int -> Ship -> Ship
trade resource price quantity =
  over balance (subtract price) .
  over inventory (Map.insertWith (+) resource quantity)

runCommand :: Command -> State World String
runCommand List =
  do ps <- use places
     return $ unlines ("Your places are..." : map (view name) ps)

runCommand (Goto n) =
  do ps <- use places
     case findPlace n ps of
       Just p ->
         ((ship . location) .= p) >>
         return ("You have moved to " ++ view name p ++ "\n" ++ view description p)
       Nothing -> return "Where's that!?!??!"


runCommand Wallet = do
   bal <- use (ship . balance)
   return $ "You have " ++ show bal ++ " currency units"

runCommand (Buy quantity resource) =
  do unitPrice <- use (here . sellPrices . at resource)
     bal <- use (ship . balance)
     case fmap (* quantity) unitPrice of
       Nothing -> return $ show resource ++ " is not for sale here"
       Just price ->
         if price > bal
            then return $ "Too pricy for you!" ++ show price
            else ship %= trade resource price quantity >>
                 return "SOLD!"

runCommand (Sell quantity resource) =
  do unitPrice <- use (here . buyPrices . at resource)
     onboard <- use (ship . inventory . at resource)
     case fmap (* quantity) unitPrice of
       Nothing -> return $ show resource ++ " is not wanted here"
       Just price ->
         if quantity >
            fromMaybe 0 onboard
            then return $ "You don't have that much " ++ show resource
            else ship %= trade resource (-price) (-quantity) >>
                 return "BOUGHT!"

runCommand Market =
  do buys  <- use (here . buyPrices)
     sells <- use (here . sellPrices)
     return $
       unlines ["Buy: " ++ show buys,"Sell: " ++ show sells]

tryRunCommand :: String -> State World String
tryRunCommand s =
  case readMaybe s of
    Just c -> runCommand c
    Nothing -> return "I'm sorry Dave, I can't do that."

initialWorld :: World
initialWorld =
  World initialMap (Ship Map.empty 20 (head initialMap))
  where initialMap :: [Place]
        initialMap =
          [Place {_name = "Eclipse"
                 ,_description = "A place where the pace of life is slow."
                 ,_buyPrices =
                    Map.fromList [(Cotton,10),(Coal,5)]
                 ,_sellPrices =
                    Map.fromList [(Amber,43),(Coal,6)]}
          ,Place {_name = "Vim"
                 ,_description = "A highly advanced society"
                 ,_buyPrices =
                    Map.fromList [(Cotton,12),(Amber,50)]
                 ,_sellPrices =
                    Map.fromList [(Amber,43),(Coal,6)]}]

run :: World -> IO World
run w = do
  input <- getLine
  let (msg, w') = runState (tryRunCommand input) w
  putStrLn msg
  run w'

main :: IO World
main =
  putStrLn "Welcome to space" >>
  run initialWorld
