{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import qualified Data.Text as T
import qualified Data.Char as C

data TokenSource = TokenSource T.Text Int Int deriving (Eq, Show)

data Token = None
           | EndLine
           | LineComment TokenSource
           | Identifier TokenSource
           | NumberLiteral TokenSource
           | StringLiteral TokenSource
           | IntType TokenSource
           | FloatType TokenSource
           | StringType TokenSource
           | BoolType TokenSource
           | FnKeyword TokenSource
           | StructKeyword TokenSource
           | LetKeyword TokenSource
           | ReturnKeyword TokenSource
           | TrueKeyword TokenSource
           | FalseKeyword TokenSource
           | BlockStart TokenSource
           | BlockEnd TokenSource
           | ParenthesisOpen TokenSource
           | ParenthesisClose TokenSource
           | ListStart TokenSource
           | ListEnd TokenSource
           | AssignOperator TokenSource
           | DotOperator TokenSource
           | CommaSeparator TokenSource
           | BooleanOr TokenSource
           | BooleanAnd TokenSource
           | BooleanNot TokenSource
           | AddOperator TokenSource
           | SubtractOperator TokenSource
           | MultiplyOperator TokenSource
           | DivideOperator TokenSource
           | EqualsComparator TokenSource
           | NotEqualsComparator TokenSource
           | LessThanComparator TokenSource
           | GreaterThanComparator TokenSource
           | LEqualsComparator TokenSource
           | GEqualsComparator TokenSource
           | ReturnsOperator TokenSource deriving (Eq, Show)

getTokenFromSrc :: T.Text -> Int -> Int -> Token
getTokenFromSrc "" _ _        = None
getTokenFromSrc "\n" _ _ = EndLine
getTokenFromSrc "\r" _ _ = EndLine
getTokenFromSrc txt line col  | isNumberLiteral txt = NumberLiteral $ TokenSource txt line col
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
getTokenFromSrc txt@"int"    line col = IntType                 $ TokenSource txt line col
getTokenFromSrc txt@"float"  line col = FloatType               $ TokenSource txt line col
getTokenFromSrc txt@"string" line col = StringType              $ TokenSource txt line col
getTokenFromSrc txt@"bool"   line col = BoolType                $ TokenSource txt line col
getTokenFromSrc txt@"fn"     line col = FnKeyword               $ TokenSource txt line col
getTokenFromSrc txt@"struct" line col = StructKeyword           $ TokenSource txt line col
getTokenFromSrc txt@"let"    line col = LetKeyword              $ TokenSource txt line col
getTokenFromSrc txt@"return" line col = ReturnKeyword           $ TokenSource txt line col
getTokenFromSrc txt@"true"   line col = TrueKeyword             $ TokenSource txt line col
getTokenFromSrc txt@"false"  line col = FalseKeyword            $ TokenSource txt line col
getTokenFromSrc txt@"{"      line col = BlockStart              $ TokenSource txt line col
getTokenFromSrc txt@"}"      line col = BlockEnd                $ TokenSource txt line col
getTokenFromSrc txt@"("      line col = ParenthesisOpen         $ TokenSource txt line col
getTokenFromSrc txt@")"      line col = ParenthesisClose        $ TokenSource txt line col
getTokenFromSrc txt@"["      line col = ListStart               $ TokenSource txt line col
getTokenFromSrc txt@"]"      line col = ListEnd                 $ TokenSource txt line col
getTokenFromSrc txt@"="      line col = AssignOperator          $ TokenSource txt line col
getTokenFromSrc txt@"."      line col = DotOperator             $ TokenSource txt line col
getTokenFromSrc txt@","      line col = CommaSeparator          $ TokenSource txt line col
getTokenFromSrc txt@"||"     line col = BooleanOr               $ TokenSource txt line col
getTokenFromSrc txt@"&&"     line col = BooleanAnd              $ TokenSource txt line col
getTokenFromSrc txt@"!"      line col = BooleanNot              $ TokenSource txt line col
getTokenFromSrc txt@"+"      line col = AddOperator             $ TokenSource txt line col
getTokenFromSrc txt@"-"      line col = SubtractOperator        $ TokenSource txt line col
getTokenFromSrc txt@"*"      line col = MultiplyOperator        $ TokenSource txt line col
getTokenFromSrc txt@"/"      line col = DivideOperator          $ TokenSource txt line col
getTokenFromSrc txt@"=="     line col = EqualsComparator        $ TokenSource txt line col
getTokenFromSrc txt@"!="     line col = NotEqualsComparator     $ TokenSource txt line col
getTokenFromSrc txt@"<"      line col = LessThanComparator      $ TokenSource txt line col
getTokenFromSrc txt@">"      line col = GreaterThanComparator   $ TokenSource txt line col
getTokenFromSrc txt@"<="     line col = LEqualsComparator       $ TokenSource txt line col
getTokenFromSrc txt@">="     line col = GEqualsComparator       $ TokenSource txt line col
getTokenFromSrc txt@"->"     line col = ReturnsOperator         $ TokenSource txt line col
getTokenFromSrc txt          line col = Identifier              $ TokenSource txt line col

lexSource :: T.Text -> [Token]
lexSource src = filter (/= None) $ continue src "" 0 0 0 []
    where
        continue :: T.Text -> T.Text -> Int -> Int -> Int -> [Token] -> [Token]
        continue txt tok line col tokCol tokens =
            let
                appendToksAndContinue remainingTxt nextTokenSrc newTokens = continue remainingTxt nextTokenSrc line (col + 1) (col + 1) (tokens ++ newTokens)
                tokAsToken = getTokenFromSrc tok line tokCol
            in case T.uncons txt of
                Just ('\"', cs)
                    -> continueAsString cs "" line (col + 1) col (tokens ++ [tokAsToken])
                Just (' ', cs)
                    -> appendToksAndContinue cs "" [tokAsToken]
                Just ('/', cs) | lookahead1 cs == "/"
                    -> continueAsLineComment (T.tail cs) "" line col (tokens ++ [tokAsToken])
                Just (c, cs) | isDelimT (tok <> T.singleton c)
                    -> appendToksAndContinue cs "" [getTokenFromSrc (tok <> T.singleton c) line tokCol]
                Just (c, cs) | isDelimC c ->
                    if c == '\n' || c == '\r' then
                        continue cs "" (line + 1) 0 0 (tokens ++ [tokAsToken, EndLine])
                    else if isDelimT $ T.singleton c <> lookahead1 cs then -- Some length 2 delims contain length 1 delims, so need to lookahead before assuming length 1
                        continue cs (T.singleton c) line (col + 1) col (tokens ++ [tokAsToken])
                    else
                        appendToksAndContinue cs "" [tokAsToken, getTokenFromSrc (T.singleton c) line col]
                Just (c, cs)
                    -> continue cs (tok <> T.singleton c) line (col + 1) tokCol tokens
                Nothing
                    -> tokens ++ [tokAsToken]

        continueAsString :: T.Text -> T.Text -> Int -> Int -> Int -> [Token] -> [Token]
        continueAsString txt tok line col tokCol tokens = case T.uncons txt of
            Just ('\"', cs) -> continue cs "" line (col + 1) (col + 1) (tokens ++ [StringLiteral $ TokenSource tok line tokCol])
            Just (c, cs)    -> continueAsString cs (tok <> T.singleton c) line (col + 1) tokCol tokens
            Nothing         -> tokens ++ [StringLiteral $ TokenSource tok line tokCol]

        continueAsLineComment :: T.Text -> T.Text -> Int -> Int -> [Token] -> [Token]
        continueAsLineComment txt tok line tokCol tokens = case T.uncons txt of
            Just (c, cs) | c == '\n' || c == '\r' -> continue cs "" (line + 1) 0 0 (tokens ++ [LineComment $ TokenSource tok line tokCol])
            Just (c, cs)                          -> continueAsLineComment cs (tok <> T.singleton c) line tokCol tokens
            Nothing                               -> tokens ++ [LineComment $ TokenSource tok line tokCol]

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

