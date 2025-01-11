{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Formatting
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Lexer as L

data ParseError = MatchTokenFailure T.Text
                | InvalidStatement T.Text
                | InvalidType T.Text
                | InvalidCodeBlockStatement T.Text
                | InvalidExpression T.Text deriving Show

type ParseOk = ([L.TokenSource], Bool)
type ParseResult = Either ParseError ParseOk

parseTokens :: [L.TokenSource] -> ParseResult
parseTokens tokens = parseStatements (filter (\(tok, _) -> tok /= L.LineComment) tokens, True)

-- Consume >=0 newlines
parseNewLines :: ParseOk -> ParseResult
parseNewLines ((L.EndLine, _) : xs, _) = parseNewLines (xs, True)
parseNewLines ok                       = Right ok

parseStatements :: ParseOk -> ParseResult
parseStatements ok@([], _) = Right ok
parseStatements ok@(_ : _, _) =
    parseNewLines ok
    >>= parseStatement 
    >>= parseNewLines
    >>= parseStatements

parseStatement :: ParseOk -> ParseResult
parseStatement ((L.FnKeyword, _) : xs, _)                   = parseFn (xs, True)
parseStatement ((L.StructKeyword, _) : xs, _)               = parseStruct (xs, True)
parseStatement ((got, L.SourceMapping txt line col) : _, _) = Left $ InvalidStatement $ formatExpectedTokenErrorMsg "fn/struct" line col (showT got) txt
parseStatement ([], _)                                      = Left $ InvalidStatement "Expected fn/struct, got EOF"

parseFn :: ParseOk -> ParseResult
parseFn ok =
    matchToken ok L.Identifier
    >>= parseParamList
    >>= (`matchToken` L.ReturnsOperator)
    >>= parseType
    >>= parseCodeBlock

parseParamList :: ParseOk -> ParseResult
parseParamList ok =
    matchToken ok L.ParenthesisOpen
    >>= parseParams
    >>= (`matchToken` L.ParenthesisClose)

parseParams :: ParseOk -> ParseResult
parseParams ok@((L.ParenthesisClose, _) : _, _) = Right ok
parseParams ok =
    parseParam ok
    >>= \ok2 -> case ok2 of
        ((L.CommaSeparator, _) : xs, _) -> parseParams (xs, True)
        _                               -> Right ok2

parseParam :: ParseOk -> ParseResult
parseParam ok =
    matchToken ok L.Identifier
    >>= parseType

parseType :: ParseOk -> ParseResult
parseType ((L.ListStart, _) : xs, _) =
    parseType (xs, True)
    >>= (`matchToken` L.ListEnd)
parseType ((tok, _) : xs, _)
    |  tok == L.Identifier
    || tok == L.IntType
    || tok == L.FloatType
    || tok == L.StringType
    || tok == L.BoolType = Right (xs, True)
parseType ((got, L.SourceMapping txt line col) : _, _) = Left $ InvalidType $ formatExpectedTokenErrorMsg "<type>" line col (showT got) txt
parseType ([], _) = Left $ InvalidType "Expected <type>, got EOF"

parseOptionalType :: ParseOk -> ParseResult
parseOptionalType ok@((tok, _) : _, _)
    |  tok == L.ListStart
    || tok == L.Identifier
    || tok == L.IntType
    || tok == L.FloatType
    || tok == L.StringType
    || tok == L.BoolType = parseType ok
parseOptionalType ok     = Right ok

parseCodeBlock :: ParseOk -> ParseResult
parseCodeBlock ok =
    matchToken ok L.BlockStart
    >>= (`matchToken` L.EndLine)
    >>= parseCodeBlockStatements
    >>= (`matchToken` L.BlockEnd)
    >>= (`matchToken` L.EndLine)

parseCodeBlockStatements :: ParseOk -> ParseResult
parseCodeBlockStatements ok@((L.BlockEnd, _) : _, _) = Right ok
parseCodeBlockStatements ok@((L.EndLine, _) : _, _)  =
    parseNewLines ok
    >>= parseCodeBlockStatements
parseCodeBlockStatements ok =
    parseCodeBlockStatement ok
    >>= parseCodeBlockStatements

parseCodeBlockStatement :: ParseOk -> ParseResult
parseCodeBlockStatement ((L.LetKeyword, _) : xs, _) =
    matchToken (xs, True) L.Identifier
    >>= parseOptionalType
    >>= (`matchToken` L.AssignOperator)
    >>= parseExpr
parseCodeBlockStatement ((L.ReturnKeyword, _) : xs, _) = parseExpr (xs, True)
parseCodeBlockStatement ((got, L.SourceMapping txt line col) : _, _) = Left $ InvalidCodeBlockStatement $ formatExpectedTokenErrorMsg "let/return" line col (showT got) txt
parseCodeBlockStatement ([], _) = Left $ InvalidCodeBlockStatement "Expected let/return got EOF"

parseExpr :: ParseOk -> ParseResult
parseExpr ok =
    parseBooleanOrExpr ok
    >>= \ok2 -> case ok2 of
        ((tok, _) : xs, _)
            |  tok == L.EqualsComparator
            || tok == L.GreaterThanComparator
            || tok == L.LessThanComparator
            || tok == L.GEqualsComparator
            || tok == L.LEqualsComparator -> parseExpr (xs, True)
        _                                 -> Right ok2

parseBooleanOrExpr :: ParseOk -> ParseResult
parseBooleanOrExpr  ok =
    parseBooleanAndExpr ok
    >>= \ok2 -> case ok2 of
        ((L.BooleanOrOperator, _) : xs, _) -> parseBooleanOrExpr (xs, True)
        _                                  -> Right ok2

parseBooleanAndExpr :: ParseOk -> ParseResult
parseBooleanAndExpr  ok =
    parseAddOrSubExpr ok
    >>= \ok2 -> case ok2 of
        ((L.BooleanAndOperator, _) : xs, _) -> parseBooleanAndExpr (xs, True)
        _                                   -> Right ok2

parseAddOrSubExpr :: ParseOk -> ParseResult
parseAddOrSubExpr ok =
    parseMultOrDivExpr ok
    >>= \ok2 -> case ok2 of
        ((L.AddOperator, _) : xs, _)      -> parseAddOrSubExpr (xs, True)
        ((L.SubtractOperator, _) : xs, _) -> parseAddOrSubExpr (xs, True)
        _                                 -> Right ok2

parseMultOrDivExpr :: ParseOk -> ParseResult
parseMultOrDivExpr ok =
    parseFactorExpr ok
    >>= \ok2 -> case ok2 of
        ((L.MultiplyOperator, _) : xs, _) -> parseMultOrDivExpr (xs, True)
        ((L.DivideOperator, _) : xs, _)   -> parseMultOrDivExpr (xs, True)
        _                                 -> Right ok2

parseFactorExpr :: ParseOk -> ParseResult
parseFactorExpr ((L.NumberLiteral, _) : (L.DotOperator, _) : (L.NumberLiteral, _) : xs, _) = Right (xs, True)
parseFactorExpr ((tok, _) : xs, _)
    |  tok == L.NumberLiteral
    || tok == L.StringLiteral
    || tok == L.TrueKeyword
    || tok == L.FalseKeyword = Right (xs, True)
parseFactorExpr ok@((L.Identifier, _) : _, _) =
    parseComplexIdentifier ok
parseFactorExpr ((L.ListStart, _) : xs, _) =
    parseArguments (xs, True)
    >>= (`matchToken` L.ListEnd)
parseFactorExpr ((L.ParenthesisOpen, _) : xs, _) =
    parseExpr (xs, True)
    >>= (`matchToken` L.ParenthesisClose)
parseFactorExpr ((got, L.SourceMapping txt line col) : _, _) = Left $ InvalidExpression $ formatExpectedTokenErrorMsg "<expr>" line col (showT got) txt
parseFactorExpr ([], _) = Left $ InvalidExpression "Expected <expr> got EOF"

parseComplexIdentifier :: ParseOk -> ParseResult
parseComplexIdentifier ok =
    matchToken ok L.Identifier
    >>= parseComplexIdentifierInner
    where
        parseComplexIdentifierInner :: ParseOk -> ParseResult
        parseComplexIdentifierInner ((L.ListStart, _) : xs, _) =
            parseExpr (xs, True)
            >>= (`matchToken` L.ListEnd)
            >>= parseComplexIdentifierInner
        parseComplexIdentifierInner ((L.ParenthesisOpen, _) : xs, _) =
            -- Function call
            parseArguments (xs, True)
            >>= (`matchToken` L.ParenthesisClose)
            >>= parseComplexIdentifierInner
        parseComplexIdentifierInner ((L.DotOperator, _) : xs, _) =
            matchToken (xs, True) L.Identifier
            >>= parseComplexIdentifierInner
        parseComplexIdentifierInner ok2 = Right ok2

parseArguments :: ParseOk -> ParseResult
parseArguments ok@((L.ParenthesisClose, _) : _, _) = Right ok
parseArguments ok =
    parseExpr ok
    >>= \ok2 -> case ok2 of
        ((L.CommaSeparator, _) : xs, _) -> parseArguments (xs, True)
        _                               -> Right ok2

parseStruct :: ParseOk -> ParseResult
parseStruct ok =
    matchToken ok L.Identifier
    >>= (`matchToken` L.BlockStart)
    >>= (`matchToken` L.EndLine)
    >>= parseStructStatements
    >>= (`matchToken` L.BlockEnd)
    >>= (`matchToken` L.EndLine)

parseStructStatements :: ParseOk -> ParseResult
parseStructStatements ok@((L.BlockEnd, _) : _, _) = Right ok
parseStructStatements ok@((L.EndLine, _) : _, _)  =
    parseNewLines ok
    >>= parseStructStatements
parseStructStatements ok =
    parseStructStatement ok
    >>= parseStructStatements
    
parseStructStatement :: ParseOk -> ParseResult
parseStructStatement ok =
    matchToken ok L.Identifier
    >>= parseType
    >>= (`matchToken` L.EndLine)

matchToken :: ParseOk -> L.Token -> ParseResult
matchToken ((got, _) : xs, _) expected | got == expected         = Right (xs, True)
matchToken ((got, L.SourceMapping txt line col) : _, _) expected = Left $ MatchTokenFailure $ formatExpectedTokenErrorMsg (showT expected) line col (showT got) txt
matchToken _ expected                                            = Left $ MatchTokenFailure $ "Expected " <> showT expected <> " at (?:?), got '?'"

formatExpectedTokenErrorMsg :: T.Text -> Int -> Int -> T.Text -> T.Text -> T.Text
formatExpectedTokenErrorMsg expected line col got txt =
    sformat ("Expected "%text%" at ("%int%":"%int%"), got "%text%" '"%text%"'") (LT.fromStrict expected) line col (LT.fromStrict got) (LT.fromStrict txt)

showT :: Show a => a -> T.Text
showT = T.pack . show

