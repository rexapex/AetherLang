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
                [ (L.Identifier         , (L.SourceMapping "x" 0 0))
                , (L.AddOperator        , (L.SourceMapping "+" 0 2))
                , (L.Identifier         , (L.SourceMapping "y" 0 4))
                , (L.SubtractOperator   , (L.SourceMapping "-" 0 6))
                , (L.NumberLiteral      , (L.SourceMapping "2" 0 8))
                , (L.DivideOperator     , (L.SourceMapping "/" 0 10))
                , (L.NumberLiteral      , (L.SourceMapping "3" 0 12))
                , (L.MultiplyOperator   , (L.SourceMapping "*" 0 14))
                , (L.NumberLiteral      , (L.SourceMapping "4" 0 16))]
        it "Lexes functions" $ do
            L.lexSource "fn test(x int, y float) -> bool {\n\
                        \   println(x + y)\n\
                        \   return true\n\
                        \}" `shouldBe`
                [ (L.FnKeyword          , (L.SourceMapping "fn" 0 0))
                , (L.Identifier         , (L.SourceMapping "test" 0 3))
                , (L.ParenthesisOpen    , (L.SourceMapping "(" 0 7))
                , (L.Identifier         , (L.SourceMapping "x" 0 8))
                , (L.IntType            , (L.SourceMapping "int" 0 10))
                , (L.CommaSeparator     , (L.SourceMapping "," 0 13))
                , (L.Identifier         , (L.SourceMapping "y" 0 15))
                , (L.FloatType          , (L.SourceMapping "float" 0 17))
                , (L.ParenthesisClose   , (L.SourceMapping ")" 0 22))
                , (L.ReturnsOperator    , (L.SourceMapping "->" 0 24))
                , (L.BoolType           , (L.SourceMapping "bool" 0 27))
                , (L.BlockStart         , (L.SourceMapping "{" 0 32))
                , (L.EndLine            , (L.SourceMapping "\n" 0 33))
                , (L.Identifier         , (L.SourceMapping "println" 1 3))
                , (L.ParenthesisOpen    , (L.SourceMapping "(" 1 10))
                , (L.Identifier         , (L.SourceMapping "x" 1 11))
                , (L.AddOperator        , (L.SourceMapping "+" 1 13))
                , (L.Identifier         , (L.SourceMapping "y" 1 15))
                , (L.ParenthesisClose   , (L.SourceMapping ")" 1 16))
                , (L.EndLine            , (L.SourceMapping "\n" 1 17))
                , (L.ReturnKeyword      , (L.SourceMapping "return" 2 3))
                , (L.TrueKeyword        , (L.SourceMapping "true" 2 10))
                , (L.EndLine            , (L.SourceMapping "\n" 2 14))
                , (L.BlockEnd           , (L.SourceMapping "}" 3 0))]

