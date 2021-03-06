{- TODO
 - Dicts are implemented.
 - There is a naive implementation of records (which will be used as modules)
 - Function representation is defined.
 - Add tests for block functions.
 - Start working on function value evaluation.
 - Start working on module value evaluation.
 -}

{-
Base = {
  name = Base
  date = d'2020-09-11'
  id = (fn (x) x)
  const = (fn (x) (fn (y) x))
}

id =>
  Block $
    Vector.fromList
      [
        SymbolTerm (Symbol "fn")
      , BlockTerm $
          Block $
            Vector.fromList [SymbolTerm (Symbol "x")]
      , SymbolTerm (Symbol "x")
      ]

- function evaluation is a feature of record evaluation.
- if a field of a record is a block starting with symbol 'fn'
- if its length is 3, then it gets function evaluated.
- if its length is not 3, then it is an error.

type(Base) -> {
  name = Symbol
  date = Date
  id = (t t)
  const = (t1 (t2 t1))
}

- A function is represented as a block.
-}

module Lib where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Control.Arrow (left)

import qualified Data.Vector as Vec

data Type
  = SymbolType
  | PairType Type Type
  | RecordType (Map.Map Symbol Type)
  deriving (Show, Eq, Ord)

isSymbol :: Term -> Bool
isSymbol (SymbolTerm _) = True
isSymbol _              = False

isType :: Term -> Bool
isType (TypeTerm _) = True
isType _            = False

data RecordTypeError = RecordTypeError
  { nonSymbolKeys :: [Term], nonTypeValues :: [Term] }

{- TODO: this should accept values that *evaluate* to types too -}
recordTypeFromDict :: Dict -> Either RecordTypeError Type
recordTypeFromDict (Dict map)
  | null nonSymbolKeys && null nonTypeValues =
    Right $ RecordType $ Map.fromList (List.zip rawSymbols rawTypes)
  | otherwise =
    Left $ RecordTypeError nonSymbolKeys nonTypeValues
  where
    (symbolKeys, nonSymbolKeys) = List.partition isSymbol (Map.keys map)
    (typeValues, nonTypeValues) = List.partition isType (Map.elems map)
    rawSymbols = getSymbol <$> symbolKeys
    rawTypes   = getType <$> typeValues

data Symbol = Symbol String deriving (Show, Eq, Ord)
data Pair   = Pair Term Term deriving (Show, Eq, Ord)
data Dict   = Dict (Map.Map Term Term) deriving (Show, Eq, Ord)
data Block  = Block (Vec.Vector Term) deriving (Show, Eq, Ord)
data Term
  = SymbolTerm { getSymbol :: Symbol }
  | PairTerm   { getPair   :: Pair   }
  | DictTerm   { getDict   :: Dict   }
  | BlockTerm  { getBlock  :: Block  }
  | TypeTerm   { getType   :: Type   }
  deriving (Show, Eq, Ord)

data DictErr = RepeatedKeys Term Term
             | NoSuchKey Term
             deriving (Show, Eq)

emptyDict :: Dict
emptyDict = Dict Map.empty

emptyBlock :: Block
emptyBlock = Block Vec.empty

blockFromList :: [Term] -> Block
blockFromList list = Block (Vec.fromList list)

addToDict :: Term -> Term -> Dict -> Either DictErr Dict
addToDict key value (Dict map)
  | key `Map.member` map  = Left  $ RepeatedKeys key (map Map.! key)
  | otherwise             = Right $ Dict (Map.insert key value map)

addPairToDict :: Pair -> Dict -> Either DictErr Dict
addPairToDict (Pair k v) dict = addToDict k v dict

addPairsToDict :: [Pair] -> Dict -> Either DictErr Dict
addPairsToDict []     dict = Right $ dict
addPairsToDict (p:ps) dict =
  addPairsToDict ps =<< (addPairToDict p dict)

pairFromTuple :: (Term, Term) -> Pair
pairFromTuple = uncurry Pair

dictFromPairs :: [Pair] -> Either DictErr Dict
dictFromPairs pairs = addPairsToDict pairs emptyDict

dictFromTuples :: [(Term, Term)] -> Either DictErr Dict
dictFromTuples = dictFromPairs . fmap pairFromTuple

getFromDict :: Term -> Dict -> Either DictErr Term
getFromDict key (Dict map) =
  case Map.lookup key map of
    Just value -> Right value
    Nothing    -> Left $ NoSuchKey key

someFunc :: IO ()
someFunc = putStrLn "someFunc"
