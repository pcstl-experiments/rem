{- TODO
 - Dicts are implemented.
 - Create the concept of a type and a record.
 - Define records in terms of dicts.
 - Define a representation for functions
 -}

{-
Base = {
  name = Base
  date = 2020-09-11
  id = (fn (x) x)
}

-}

module Lib where

import qualified Data.Map.Strict as Map

data Sym  = Sym String deriving (Show, Eq, Ord)
data Pair = Pair Term Term deriving (Show, Eq, Ord)
data Dict = Dict (Map.Map Term Term) deriving (Show, Eq, Ord)
data Term = SymTerm Sym
          | PairTerm Pair
          | DictTerm Dict
          deriving (Show, Eq, Ord)

data DictErr = RepeatedKeys Term Term
             | NoSuchKey Term
             deriving (Show, Eq)

emptyDict :: Dict
emptyDict = Dict Map.empty

addToDict :: Term -> Term -> Dict -> Either DictErr Dict
addToDict key value (Dict map)
  | key `Map.member` map  = Left  $ RepeatedKeys key (map Map.! key)
  | otherwise             = Right $ Dict (Map.insert key value map)

addPairToDict :: Pair -> Dict -> Either DictErr Dict
addPairToDict (Pair k v) dict = addToDict k v dict

getFromDict :: Term -> Dict -> Either DictErr Term
getFromDict key (Dict map) =
  case Map.lookup key map of
    Just value -> Right value
    Nothing    -> Left $ NoSuchKey key

someFunc :: IO ()
someFunc = putStrLn "someFunc"
