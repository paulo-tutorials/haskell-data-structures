module TreeSpec where

import Test.Hspec

import Tree

spec :: Spec
spec = context "using Set as element" $ do
    describe "verify that `a`" $ do
        it "is member of tree" $ do
            member 1 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` True
        it "is not member of tree" $ do
            member 10 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` False
    describe "insert an item a" $ do
        it "in a tree that already contain it" $ do
            insert 7 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T E 3 E) 5 (T (T E 7 E) 15 E))
        it "in a tree that do not contain it" $ do
            insert 0 (T (T E 3 E) 5 (T (T E 7 E) 15 E))
                `shouldBe`
                    (T (T (T E 0 E) 3 E) 5 (T (T E 7 E) 15 E))
    describe "verify that `a` (using member d+1 worst case)" $ do
        it "is member of a tree" $ do
            member' 1 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` True
        it "is not member of tree" $ do
            member' 10 (T (T E 1 E) 3 (T E 7 E))
                `shouldBe` False