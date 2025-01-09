{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import qualified Data.Text as T
import qualified Data.Char as C

data Token = None
           | EndLine
           | LineComment T.Text
           | Identifier T.Text
           | NumberLiteral T.Text
           | StringLiteral T.Text
           | IntType
           | FloatType
           | StringType
           | BoolType
           | FnKeyword
           | StructKeyword
           | LetKeyword
           | ReturnKeyword
           | TrueKeyword
           | FalseKeyword
           | BlockStart
           | BlockEnd
           | ParenthesisOpen
           | ParenthesisClose
           | ListStart
           | ListEnd
           | AssignOperator
           | DotOperator
           | CommaSeparator
           | BooleanOr
           | BooleanAnd
           | BooleanNot
           | AddOperator
           | SubtractOperator
           | MultiplyOperator
           | DivideOperator
           | EqualsComparator
           | NotEqualsComparator
           | LessThanComparator
           | GreaterThanComparator
           | LEqualsComparator
           | GEqualsComparator
           | ReturnsOperator deriving (Eq, Show)

getTokenFromSrc :: T.Text -> Token
getTokenFromSrc "" = None
getTokenFromSrc "\n" = EndLine
getTokenFromSrc "\r" = EndLine
getTokenFromSrc txt | isNumberLiteral txt = NumberLiteral txt
    where
        isNumberLiteral :: T.Text -> Bool
        isNumberLiteral txt2 = case T.uncons txt2  of
            Just(c, cs) | C.isDigit c -> isNumberLiteralLHS cs
            _                         -> False
        isNumberLiteralLHS :: T.Text -> Bool
        isNumberLiteralLHS txt2 = case T.uncons txt2 of
            Just(c, cs) | C.isDigit c -> isNumberLiteralLHS cs
            Just('.', cs)             -> isNumberLiteralRHS cs
            Just _                    -> False
            _                         -> True
        -- TODO - RHS is currently unused as '.' is always interpreted as DotOperator
        isNumberLiteralRHS :: T.Text -> Bool
        isNumberLiteralRHS txt2 = case T.uncons txt2 of
            Just(c, cs) | C.isDigit c -> isNumberLiteralRHS cs
            Just _                    -> False
            _                         -> True
-- StringLiteral is handled separately so not needed here
getTokenFromSrc "int" = IntType
getTokenFromSrc "float" = FloatType
getTokenFromSrc "string" = StringType
getTokenFromSrc "bool" = BoolType
getTokenFromSrc "fn" = FnKeyword
getTokenFromSrc "struct" = StructKeyword
getTokenFromSrc "let" = LetKeyword
getTokenFromSrc "return" = ReturnKeyword
getTokenFromSrc "true" = TrueKeyword
getTokenFromSrc "false" = FalseKeyword
getTokenFromSrc "{" = BlockStart
getTokenFromSrc "}" = BlockEnd
getTokenFromSrc "(" = ParenthesisOpen
getTokenFromSrc ")" = ParenthesisClose
getTokenFromSrc "[" = ListStart
getTokenFromSrc "]" = ListEnd
getTokenFromSrc "=" = AssignOperator
getTokenFromSrc "." = DotOperator
getTokenFromSrc "," = CommaSeparator
getTokenFromSrc "||" = BooleanOr
getTokenFromSrc "&&" = BooleanAnd
getTokenFromSrc "!" = BooleanNot
getTokenFromSrc "+" = AddOperator
getTokenFromSrc "-" = SubtractOperator
getTokenFromSrc "*" = MultiplyOperator
getTokenFromSrc "/" = DivideOperator
getTokenFromSrc "==" = EqualsComparator
getTokenFromSrc "!=" = NotEqualsComparator
getTokenFromSrc "<" = LessThanComparator
getTokenFromSrc ">" = GreaterThanComparator
getTokenFromSrc "<=" = LEqualsComparator
getTokenFromSrc ">=" = GEqualsComparator
getTokenFromSrc "->" = ReturnsOperator
getTokenFromSrc txt = Identifier txt

lexSource :: T.Text -> [Token]
lexSource src = filter (/= None) $ processChar src "" []
    where
        processChar :: T.Text -> T.Text -> [Token] -> [Token]
        processChar txt tok tokens = case T.uncons txt of
            Just ('\"', cs)
                -> formString cs "" (tokens ++ [getTokenFromSrc tok])
            Just (' ', cs)
                -> processChar cs "" (tokens ++ [getTokenFromSrc tok])
            Just ('/', cs) | lookahead1 cs == "/"
                -> formLineComment (T.tail cs) "" (tokens ++ [getTokenFromSrc tok])
            Just (c, cs) | isDelimT (tok <> T.singleton c)
                -> processChar cs "" (tokens ++ [getTokenFromSrc $ tok <> T.singleton c])
            Just (c, cs) | isDelimC c
                -> if isDelimT $ T.singleton c <> lookahead1 cs then -- Some length 2 delims contain length 1 delims, so need to lookahead before assuming length 1
                       processChar cs (T.singleton c) (tokens ++ [getTokenFromSrc tok])
                   else
                       processChar cs "" (tokens ++ [getTokenFromSrc tok, getTokenFromSrc $ T.singleton c])
            Just (c, cs)
                -> processChar cs (tok <> T.singleton c) tokens
            Nothing
                -> tokens ++ [getTokenFromSrc tok]

        formString :: T.Text -> T.Text -> [Token] -> [Token]
        formString txt tok tokens = case T.uncons txt of
            Just ('\"', cs) -> processChar cs "" (tokens ++ [StringLiteral tok])
            Just (c, cs)    -> formString cs (tok <> T.singleton c) tokens
            Nothing         -> tokens ++ [StringLiteral tok]

        formLineComment :: T.Text -> T.Text -> [Token] -> [Token]
        formLineComment txt tok tokens = case T.uncons txt of
            Just (c, cs) | c == '\n' || c == '\r' -> processChar cs "" (tokens ++ [LineComment tok])
            Just (c, cs)                          -> formLineComment cs (tok <> T.singleton c) tokens
            Nothing                               -> tokens ++ [LineComment tok]

        lookahead1 :: T.Text -> T.Text
        lookahead1 txt = case T.uncons txt of
            Just (c, _) -> T.singleton c
            Nothing     -> ""

        isDelimC :: Char -> Bool
        isDelimC '\n' = True
        isDelimC '\r' = True
        isDelimC '\t' = True
        isDelimC '{'  = True
        isDelimC '}'  = True
        isDelimC '('  = True
        isDelimC ')'  = True
        isDelimC '['  = True
        isDelimC ']'  = True
        isDelimC '.'  = True
        isDelimC ','  = True
        isDelimC '!'  = True
        isDelimC '+'  = True
        isDelimC '-'  = True
        isDelimC '*'  = True
        isDelimC '/'  = True
        isDelimC '<'  = True
        isDelimC '>'  = True
        isDelimC _    = False

        -- NOTE - The way this is used only allows for length 2 delims
        isDelimT :: T.Text -> Bool
        isDelimT "||" = True
        isDelimT "&&" = True
        isDelimT "==" = True
        isDelimT "!=" = True
        isDelimT "<=" = True
        isDelimT ">=" = True
        isDelimT "->" = True
        isDelimT _    = False

