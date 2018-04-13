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
    let context = newContext
    let arg = ["abc", "def", "ghi"]
    --t <- runQ [t| [String] |]
    let t = AppT ListT $ ConT $ mkName "String"
    r <- renderRuntime context "data/test/main.txt" $(currentModules) (arg, "it", t) cache
    putStrLn r