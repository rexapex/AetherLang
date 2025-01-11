{-# LANGUAGE OverloadedStrings #-}
-- LANGUAGE LambdaCase #-}

module Aether (aether, compileSource) where

import qualified Data.Text as T

import qualified Lexer as L
import qualified Parser as P

aether :: IO ()
aether = do
    putStrLn "Aether!"
    compileSource "struct X {\n\
                  \   \n\
                  \   x int\n\
                  \   \n\
                  \   \n\
                  \   y float\n\
                  \}\n\
                  \fn main(argc int, argv [string]) -> int {\n\
                  \    let w=x   + y - 8 // Comment x+y==2\n\
                  \    let world [[[int]]] = [[[0, 1, 2]]]\n\
                  \    let l = [2, 3, 4, fac(5)]\n\
                  \    let s = \"Hello World! Just called fac(5)\"\n\
                  \    let d = a.b.c[w.w][z].e()[1]\n\
                  \    return a+b-c/27.432\n\
                  \}\n\
                  \fn fac(x int) -> X {\n\
                  \    let _ = println(x, y, z)\n\
                  \}\n"
    return ()

compileSource :: T.Text -> IO ()
compileSource txt = do
    let tokens = L.lexSource txt
    putStrLn "\n\n\n====Tokens====\n"
    mapM_ print tokens

    let result = P.parseTokens tokens
    putStrLn "\n\n\n====Parse====\n"
    case result of
        Left err -> print err
        Right ok -> print ok
    --mapM_ print result

