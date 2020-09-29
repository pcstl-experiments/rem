import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck ((==>), testProperty)

import qualified Lib

main :: IO ()
main = defaultMain tests

symbolTerm :: String -> Lib.Term
symbolTerm = Lib.SymbolTerm . Lib.Symbol

symbolPair :: String -> String -> Lib.Term
symbolPair s1 s2 = Lib.PairTerm $ Lib.Pair (symbolTerm s1) (symbolTerm s2)

tests :: TestTree
tests = testGroup "All tests"
  [ testProperty "Adding to dict works for different keys" $
      \k1 v1 k2 v2 ->
        k1 /= k2 ==>
          let key1    = symbolTerm k1
              key2    = symbolTerm k2
              value1  = symbolTerm v1
              value2  = symbolTerm v2
              result  = Lib.addToDict key1 value1 Lib.emptyDict
              result' = Lib.addToDict key2 value2 =<< result
          in case result' of
            Right _ -> True
            Left  _ -> False
  , testProperty "Adding to dict fails for equal keys" $
      \k v ->
        let key     = symbolTerm k
            value   = symbolTerm v
            result  = Lib.addToDict key value Lib.emptyDict
            result' = Lib.addToDict key value =<< result
        in case result' of
          Right _ -> False
          Left  _ -> True
  , testProperty "Adding does not depend on order" $
      \k1 v1 k2 v2 ->
        k1 /= k2 ==>
          let key1     = symbolTerm k1
              key2     = symbolTerm k2
              value1   = symbolTerm v1
              value2   = symbolTerm v2
              result1  = Lib.addToDict key1 value1 Lib.emptyDict
              result1' = Lib.addToDict key2 value2 =<< result1
              result2  = Lib.addToDict key2 value2 Lib.emptyDict
              result2' = Lib.addToDict key1 value1 =<< result2
          in case
            (do
                d1 <- result1'
                d2 <- result2'
                pure $ d1 == d2
            ) of
               Right True -> True
               Left  _    -> False
  , testProperty "Getting from map with single key works" $
      \k v ->
        let key    = symbolTerm k
            value  = symbolTerm v
            dict   = Lib.addToDict key value Lib.emptyDict
        in case Lib.getFromDict key =<< dict of
          Right obtainedValue -> obtainedValue == value
          Left  _             -> False
  , testProperty "Getting from map with no keys does not work" $
     \k ->
       let key = symbolTerm k
       in case Lib.getFromDict key Lib.emptyDict of
         Right _                   -> False
         Left (Lib.NoSuchKey key') -> key == key'
  , testProperty "Dict with symbol keys and type values can be used as record type" $
    \s1 s2 ->
      s1 /= s2 ==>
        let k1         = symbolTerm s1
            k2         = symbolTerm s2
            v          = Lib.TypeTerm Lib.SymbolType
            resultDict = Lib.dictFromPairs [Lib.Pair k1 v, Lib.Pair k2 v]
        in case resultDict of
          Left _     -> False
          Right dict ->
            case Lib.recordTypeFromDict dict of
              Left _  -> False
              Right _ -> True
  , testProperty "Dict with a non-symbol key cannot be used as record type" $
    \s1 s2 ->
        let k1         = symbolPair s1 s1
            k2         = symbolTerm s2
            v          = Lib.TypeTerm Lib.SymbolType
            resultDict = Lib.dictFromPairs [Lib.Pair k1 v, Lib.Pair k2 v]
        in case resultDict of
          Left _     -> False
          Right dict ->
            case Lib.recordTypeFromDict dict of
              Right _  -> False
              Left err -> Lib.nonSymbolKeys err == [k1]
  ]
