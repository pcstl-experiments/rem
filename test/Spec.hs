import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck ((==>), testProperty)

import qualified Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
  [ testProperty "Adding to dict works for different keys" $
      \k1 v1 k2 v2 ->
        k1 /= k2 ==>
          let key1    = Lib.SymTerm (Lib.Sym k1)
              key2    = Lib.SymTerm (Lib.Sym k2)
              value1  = Lib.SymTerm (Lib.Sym v1)
              value2  = Lib.SymTerm (Lib.Sym v2)
              result  = Lib.addToDict key1 value1 Lib.emptyDict
              result' = Lib.addToDict key2 value2 =<< result
          in case result' of
            Right _ -> True
            Left  _ -> False
  , testProperty "Adding to dict fails for equal keys" $
      \k v ->
        let key     = Lib.SymTerm (Lib.Sym k)
            value   = Lib.SymTerm (Lib.Sym v)
            result  = Lib.addToDict key value Lib.emptyDict
            result' = Lib.addToDict key value =<< result
        in case result' of
          Right _ -> False
          Left  _ -> True
  , testProperty "Adding does not depend on order" $
      \k1 v1 k2 v2 ->
        k1 /= k2 ==>
          let key1     = Lib.SymTerm (Lib.Sym k1)
              key2     = Lib.SymTerm (Lib.Sym k2)
              value1   = Lib.SymTerm (Lib.Sym v1)
              value2   = Lib.SymTerm (Lib.Sym v2)
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
        let key    = Lib.SymTerm (Lib.Sym k)
            value  = Lib.SymTerm (Lib.Sym v)
            dict   = Lib.addToDict key value Lib.emptyDict
        in case Lib.getFromDict key =<< dict of
          Right obtainedValue -> obtainedValue == value
          Left  _             -> False
  , testProperty "Getting from map with no keys does not work" $
     \k ->
       let key = Lib.SymTerm (Lib.Sym k) 
       in case Lib.getFromDict key Lib.emptyDict of
         Right _                   -> False
         Left (Lib.NoSuchKey key') -> key == key'
  ]
