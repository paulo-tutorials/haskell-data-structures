module UnbalancedSetSpec where

import Test.Hspec

import UnbalancedSet
import qualified Set.Set as S

spec :: Spec
spec = context "using Set as element" $ do
    describe "verify that `a`" $ do
        it "is member of UnbalancedSet" $ do
            S.member 1 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` True
        it "is not member of UnbalancedSet" $ do
            S.member 10 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` False
    describe "insert an item a" $ do
        it "in a UnbalancedSet that already contain it" $ do
            S.insert 7 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T E 3 E) 5 (T (T E 7 E) 15 E))
        it "in a UnbalancedSet that do not contain it" $ do
            S.insert 0 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T (T E 0 E) 3 E) 5 (T (T E 7 E) 15 E))
    describe "verify that `a` (worst case d+1)" $ do
        it "is member of a UnbalancedSet" $ do
            member' 1 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` True
        it "is not member of UnbalancedSet" $ do
            member' 10 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` False
    describe "insert an item a (ex 2.3)" $ do
        it "in a UnbalancedSet that already contain it" $ do
            insert' 7 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T E 3 E) 5 (T (T E 7 E) 15 E))
        it "in a UnbalancedSet that do not contain it" $ do
            insert' 0 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T (T E 0 E) 3 E) 5 (T (T E 7 E) 15 E))