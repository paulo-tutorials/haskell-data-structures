module StackSpec where

import Test.Hspec

import Stack

spec :: Spec
spec = context "basics operation on Stack" $ do
    describe "isEmpty" $ do
        it "returns true when pass an empty stack" $ do
            isEmpty Empty
                `shouldBe`
                    True
        it "returns false when pass a non empty stack" $ do
            isEmpty (Cons "beep" Empty)
                `shouldBe`
                    False

    describe "cons between a elementan and" $ do
        it "empty stack produces stack with one element" $ do
            cons "abc" Empty
                `shouldBe`
                    Cons "abc" Empty
        it "non empty stack produces stack with stack" $ do
            cons "wow" (Cons "very" (Cons "stack" Empty))
                `shouldBe`
                    Cons "wow" (Cons "very" (Cons "stack" Empty))

    describe "shead" $ do
        it "returns the first element of stack" $ do
            shead (Cons 42 (Cons 71 Empty))
                `shouldBe`
                    42
    describe "stail" $ do
        it "returns tail of stack" $ do
            stail (Cons 42 (Cons 71 Empty))
                `shouldBe`
                    (Cons 71 Empty)