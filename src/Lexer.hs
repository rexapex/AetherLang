{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import qualified Data.Text as T
import qualified Data.Char as C

data SourceMapping = SourceMapping T.Text Int Int deriving (Eq, Show)

data Token = None
           | EndLine
           | LineComment
           | Identifier
           | NumberLiteral
           | StringLiteral
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
           | BooleanOrOperator
           | BooleanAndOperator
           | BooleanNotOperator
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

type TokenSource = (Token, SourceMapping)

getTokenFromSrc :: T.Text -> Int -> Int -> TokenSource
getTokenFromSrc txt@"" line col = (None, SourceMapping txt line col)
getTokenFromSrc "\n" line col   = (EndLine, SourceMapping "\n" line col)
getTokenFromSrc "\r" line col   = (EndLine, SourceMapping "\n" line col)
getTokenFromSrc txt line col | isNumberLiteral txt = (NumberLiteral, SourceMapping txt line col)
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
getTokenFromSrc txt@"int"    line col = (IntType               , SourceMapping txt line col)
getTokenFromSrc txt@"float"  line col = (FloatType             , SourceMapping txt line col)
getTokenFromSrc txt@"string" line col = (StringType            , SourceMapping txt line col)
getTokenFromSrc txt@"bool"   line col = (BoolType              , SourceMapping txt line col)
getTokenFromSrc txt@"fn"     line col = (FnKeyword             , SourceMapping txt line col)
getTokenFromSrc txt@"struct" line col = (StructKeyword         , SourceMapping txt line col)
getTokenFromSrc txt@"let"    line col = (LetKeyword            , SourceMapping txt line col)
getTokenFromSrc txt@"return" line col = (ReturnKeyword         , SourceMapping txt line col)
getTokenFromSrc txt@"true"   line col = (TrueKeyword           , SourceMapping txt line col)
getTokenFromSrc txt@"false"  line col = (FalseKeyword          , SourceMapping txt line col)
getTokenFromSrc txt@"{"      line col = (BlockStart            , SourceMapping txt line col)
getTokenFromSrc txt@"}"      line col = (BlockEnd              , SourceMapping txt line col)
getTokenFromSrc txt@"("      line col = (ParenthesisOpen       , SourceMapping txt line col)
getTokenFromSrc txt@")"      line col = (ParenthesisClose      , SourceMapping txt line col)
getTokenFromSrc txt@"["      line col = (ListStart             , SourceMapping txt line col)
getTokenFromSrc txt@"]"      line col = (ListEnd               , SourceMapping txt line col)
getTokenFromSrc txt@"="      line col = (AssignOperator        , SourceMapping txt line col)
getTokenFromSrc txt@"."      line col = (DotOperator           , SourceMapping txt line col)
getTokenFromSrc txt@","      line col = (CommaSeparator        , SourceMapping txt line col)
getTokenFromSrc txt@"||"     line col = (BooleanOrOperator     , SourceMapping txt line col)
getTokenFromSrc txt@"&&"     line col = (BooleanAndOperator    , SourceMapping txt line col)
getTokenFromSrc txt@"!"      line col = (BooleanNotOperator    , SourceMapping txt line col)
getTokenFromSrc txt@"+"      line col = (AddOperator           , SourceMapping txt line col)
getTokenFromSrc txt@"-"      line col = (SubtractOperator      , SourceMapping txt line col)
getTokenFromSrc txt@"*"      line col = (MultiplyOperator      , SourceMapping txt line col)
getTokenFromSrc txt@"/"      line col = (DivideOperator        , SourceMapping txt line col)
getTokenFromSrc txt@"=="     line col = (EqualsComparator      , SourceMapping txt line col)
getTokenFromSrc txt@"!="     line col = (NotEqualsComparator   , SourceMapping txt line col)
getTokenFromSrc txt@"<"      line col = (LessThanComparator    , SourceMapping txt line col)
getTokenFromSrc txt@">"      line col = (GreaterThanComparator , SourceMapping txt line col)
getTokenFromSrc txt@"<="     line col = (LEqualsComparator     , SourceMapping txt line col)
getTokenFromSrc txt@">="     line col = (GEqualsComparator     , SourceMapping txt line col)
getTokenFromSrc txt@"->"     line col = (ReturnsOperator       , SourceMapping txt line col)
getTokenFromSrc txt          line col = (Identifier            , SourceMapping txt line col)

lexSource :: T.Text -> [TokenSource]
lexSource src = filter (\(tok, _) -> tok /= None) $ continue src "" 0 0 0 []
    where
        continue :: T.Text -> T.Text -> Int -> Int -> Int -> [TokenSource] -> [TokenSource]
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
                        continue cs "" (line + 1) 0 0 (tokens ++ [tokAsToken, (EndLine, SourceMapping "\n" line col)])
                    else if isDelimT $ T.singleton c <> lookahead1 cs then -- Some length 2 delims contain length 1 delims, so need to lookahead before assuming length 1
                        continue cs (T.singleton c) line (col + 1) col (tokens ++ [tokAsToken])
                    else
                        appendToksAndContinue cs "" [tokAsToken, getTokenFromSrc (T.singleton c) line col]
                Just (c, cs)
                    -> continue cs (tok <> T.singleton c) line (col + 1) tokCol tokens
                Nothing
                    -> tokens ++ [tokAsToken]

        continueAsString :: T.Text -> T.Text -> Int -> Int -> Int -> [TokenSource] -> [TokenSource]
        continueAsString txt tok line col tokCol tokens = case T.uncons txt of
            Just ('\"', cs) -> continue cs "" line (col + 1) (col + 1) (tokens ++ [(StringLiteral, SourceMapping tok line tokCol)])
            Just (c, cs)    -> continueAsString cs (tok <> T.singleton c) line (col + 1) tokCol tokens
            Nothing         -> tokens ++ [(StringLiteral, SourceMapping tok line tokCol)]

        continueAsLineComment :: T.Text -> T.Text -> Int -> Int -> [TokenSource] -> [TokenSource]
        continueAsLineComment txt tok line tokCol tokens = case T.uncons txt of
            Just (c, cs) | c == '\n' || c == '\r' -> continue cs "" (line + 1) 0 0 (tokens ++ [(LineComment, SourceMapping tok line tokCol)])
            Just (c, cs)                          -> continueAsLineComment cs (tok <> T.singleton c) line tokCol tokens
            Nothing                               -> tokens ++ [(LineComment, SourceMapping tok line tokCol)]

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
        isDelimC '='  = True
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

