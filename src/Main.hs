module Main (main) where

import           SpaceTrader

main :: IO World
main =
  putStrLn "Welcome to space" >>
  run initialWorld
