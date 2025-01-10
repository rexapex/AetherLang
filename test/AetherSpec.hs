{-# LANGUAGE OverloadedStrings #-}

module AetherSpec where

import Test.Hspec
--import Test.QuickCheck

import qualified Lexer as L

spec :: IO ()
spec = hspec $ do
    describe "Aether.Lexer" $ do
        it "Lexes arithmetic" $ do
            L.lexSource "x + y - 2 / 3 * 4" `shouldBe`
                [ L.Identifier "x"
                , L.AddOperator
                , L.Identifier "y"
                , L.SubtractOperator
                , L.NumberLiteral "2"
                , L.DivideOperator
                , L.NumberLiteral "3"
                , L.MultiplyOperator
                , L.NumberLiteral "4"]
        it "Lexes functions" $ do
            L.lexSource "fn test(x int, y float) -> bool {\n\
                        \   println(x + y)\n\
                        \   return true\n\
                        \}" `shouldBe`
                [ L.FnKeyword
                , L.Identifier "test"
                , L.ParenthesisOpen
                , L.Identifier "x"
                , L.IntType
                , L.CommaSeparator
                , L.Identifier "y"
                , L.FloatType
                , L.ParenthesisClose
                , L.ReturnsOperator
                , L.BoolType
                , L.BlockStart
                , L.EndLine
                , L.Identifier "println"
                , L.ParenthesisOpen
                , L.Identifier "x"
                , L.AddOperator
                , L.Identifier "y"
                , L.ParenthesisClose
                , L.EndLine
                , L.ReturnKeyword
                , L.TrueKeyword
                , L.EndLine
                , L.BlockEnd]


