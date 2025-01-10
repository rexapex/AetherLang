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
                [ L.Identifier          (L.TokenSource "x" 0 0)
                , L.AddOperator         (L.TokenSource "+" 0 2)
                , L.Identifier          (L.TokenSource "y" 0 4)
                , L.SubtractOperator    (L.TokenSource "-" 0 6)
                , L.NumberLiteral       (L.TokenSource "2" 0 8)
                , L.DivideOperator      (L.TokenSource "/" 0 10)
                , L.NumberLiteral       (L.TokenSource "3" 0 12)
                , L.MultiplyOperator    (L.TokenSource "*" 0 14)
                , L.NumberLiteral       (L.TokenSource "4" 0 16)]
        it "Lexes functions" $ do
            L.lexSource "fn test(x int, y float) -> bool {\n\
                        \   println(x + y)\n\
                        \   return true\n\
                        \}" `shouldBe`
                [ L.FnKeyword           (L.TokenSource "fn" 0 0)
                , L.Identifier          (L.TokenSource "test" 0 3)
                , L.ParenthesisOpen     (L.TokenSource "(" 0 7)
                , L.Identifier          (L.TokenSource "x" 0 8)
                , L.IntType             (L.TokenSource "int" 0 10)
                , L.CommaSeparator      (L.TokenSource "," 0 13)
                , L.Identifier          (L.TokenSource "y" 0 15)
                , L.FloatType           (L.TokenSource "float" 0 17)
                , L.ParenthesisClose    (L.TokenSource ")" 0 22)
                , L.ReturnsOperator     (L.TokenSource "->" 0 24)
                , L.BoolType            (L.TokenSource "bool" 0 27)
                , L.BlockStart          (L.TokenSource "{" 0 32)
                , L.EndLine
                , L.Identifier          (L.TokenSource "println" 1 3)
                , L.ParenthesisOpen     (L.TokenSource "(" 1 10)
                , L.Identifier          (L.TokenSource "x" 1 11)
                , L.AddOperator         (L.TokenSource "+" 1 13)
                , L.Identifier          (L.TokenSource "y" 1 15)
                , L.ParenthesisClose    (L.TokenSource ")" 1 16)
                , L.EndLine
                , L.ReturnKeyword       (L.TokenSource "return" 2 3)
                , L.TrueKeyword         (L.TokenSource "true" 2 10)
                , L.EndLine
                , L.BlockEnd            (L.TokenSource "}" 3 0)]

