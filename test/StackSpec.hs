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

    describe "update" $ do
        it "the first element of the stack" $ do
            update (Cons "olar" (Cons "update" Empty)) 0 "such"
                `shouldBe`
                    (Cons "such" (Cons "update" Empty))
        it "the middle element of the stack" $ do
            update (Cons "such" (Cons "abc" (Cons "wow" Empty))) 1 "update"
                `shouldBe`
                    (Cons "such" (Cons "update" (Cons "wow" Empty)))

    describe "+++ (concatenate operation)" $ do
        it "return a five elements stack when pass a two elements and a three elements stack" $ do
            (Cons True (Cons False Empty)) +++ (Cons False (Cons True (Cons True Empty)))
                `shouldBe`
                    (Cons True (Cons False (Cons False (Cons True (Cons True Empty)))))
        it "returns the same result when the first or second parameter is Empty" $ do
            (Cons 41 Empty) +++ Empty
                `shouldBe`
                    Empty +++ (Cons 41 Empty)
        it "returns a empty stack when both parameters are Empty" $ do
            Empty +++ Empty
                `shouldBe`
                    (stail (Cons 1 Empty))
    describe "obtain the sufixes" $ do
        it "of [1, 2, 3, 4]" $ do
            sufixes [1, 2, 3, 4]
                `shouldBe`
		[[1, 2, 3, 4], [2, 3, 4], [3, 4], [4], []]
