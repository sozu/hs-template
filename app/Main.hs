{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Time
import Data.IORef
import qualified Data.List as L
import Language.Haskell.TH
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Data.Template

main :: IO ()
main = do
    cache <- newIORef Nothing
    let context = newContext += ("s", [| "abc" |])
    r <- renderRuntime context "data/test/out.txt" $(currentModules) cache
    putStrLn r