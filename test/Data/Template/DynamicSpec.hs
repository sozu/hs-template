{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE IncoherentInstances #-}

module Data.Template.DynamicSpec where

import Test.Hspec
import System.IO
import Language.Haskell.TH
import Data.IORef
import Data.Map as M
import Data.Template.Model
import Data.Template.Dynamic

testTemplate = "data/test/dynamic.txt"

spec :: Spec
spec = do
    describe "Render template dynamically" $ do
        it "Render once" $ do
            writeFile testTemplate "{- s -}" 
            cache <- newIORef Nothing
            exp0 <- runQ $ renderDynamic testTemplate cache
            Just (_, exp1) <- readIORef cache
            exp0 `shouldBe` exp1

        it "Render twice" $ do
            writeFile testTemplate "{- s -}" 
            cache <- newIORef Nothing
            exp0 <- runQ $ renderDynamic testTemplate cache
            (exp1, loaded) <- runQ $ renderDynamic' newContext testTemplate cache
            exp0 `shouldBe` exp1
            loaded `shouldBe` False

        it "Render after change" $ do
            writeFile testTemplate "{- s -}" 
            cache <- newIORef Nothing
            exp0 <- runQ $ renderDynamic testTemplate cache
            writeFile testTemplate "{- t -}" 
            (exp1, loaded) <- runQ $ renderDynamic' newContext testTemplate cache
            exp0 `shouldNotBe` exp1
            loaded `shouldBe` True

