module TreeSpec where

import Test.Hspec

import Tree
import qualified Set.Set as S

spec :: Spec
spec = context "using Set as element" $ do
    describe "verify that `a`" $ do
        it "is member of tree" $ do
            S.member 1 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` True
        it "is not member of tree" $ do
            S.member 10 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` False
    describe "insert an item a" $ do
        it "in a tree that already contain it" $ do
            S.insert 7 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T E 3 E) 5 (T (T E 7 E) 15 E))
        it "in a tree that do not contain it" $ do
            S.insert 0 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T (T E 0 E) 3 E) 5 (T (T E 7 E) 15 E))