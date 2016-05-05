module TreeSpec where

import Test.Hspec

import Tree

spec :: Spec
spec = context "using Set as element" $ do
    describe "verify that `a`" $ do
        it "is member of tree" $ do
            member 1 (Elem 3 (Elem 1 Empty Empty) (Elem 7 Empty Empty))
                `shouldBe` True
        it "is not member of tree" $ do
            member 10 (Elem 3 (Elem 1 Empty Empty) (Elem 7 Empty Empty))
                `shouldBe` False
    describe "insert an item a" $ do
        it "in a tree that already contain it" $ do
            insert 7 (Elem 5 (Elem 3 Empty Empty) (Elem 15 (Elem 7 Empty Empty) Empty))
                `shouldBe`
                    (Elem 5 (Elem 3 Empty Empty) (Elem 15 (Elem 7 Empty Empty) Empty))
        it "in a tree that do not contain it" $ do
            insert 0 (Elem 5 (Elem 3 Empty Empty) (Elem 15 Empty Empty))
                `shouldBe`
                    (Elem 5 (Elem 3 (Elem 0 Empty Empty) Empty) (Elem 15 Empty Empty))