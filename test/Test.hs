module Main where

import           Control.Applicative
import           Control.Monad.State
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           SpaceTrader
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

instance (Ord k,Arbitrary k,Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = Map.fromList <$> arbitrary

instance Arbitrary Resource where
  arbitrary = elements [Coal,Cotton,Amber]

instance Arbitrary Place where
  arbitrary = Place
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary Ship where
  arbitrary = Ship
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary World where
  arbitrary = World
              <$> arbitrary
              <*> arbitrary

properties :: TestTree
properties =
  testGroup "(checked by QuickCheck)"
            [QC.testProperty "List command cannot change the world." $
             \world ->
               world ==
               execState (runCommand List) world
            ,QC.testProperty "Market command cannot change the world." $
             \world ->
               world ==
               execState (runCommand Market) world
            ,QC.testProperty "Wallet command cannot change the world." $
             \world ->
               world ==
               execState (runCommand Wallet) world]
