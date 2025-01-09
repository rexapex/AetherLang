{-# LANGUAGE OverloadedStrings #-}
-- LANGUAGE LambdaCase #-}

module Aether (aether, compileSource) where

import qualified Data.Text as T

import qualified Lexer as L

aether :: IO ()
aether = do
    putStrLn "Aether!"
    compileSource "x   + y - 8 // Comment x+y==2\n\
                  \let z int = p\n\
                  \a+b-c/27.432\n\
                  \fn fac(x int) {\n\
                  \    println(x, y, z)\n\
                  \}"
    return ()

compileSource :: T.Text -> IO ()
compileSource txt = do
    let tokens = L.lexSource txt
    print $ length tokens
    mapM_ print tokens

