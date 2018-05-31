{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Template.Dynamic where

import GHC hiding (Module, ValD, Type)
import GHC.Paths
import GHC.LanguageExtensions.Type
import DynFlags
import System.Directory
import Control.Applicative
import Control.Monad.IO.Class
import Data.Dynamic
import Data.IORef
import Data.Time
import Data.Data
import Data.Maybe (fromJust, catMaybes)
import qualified Data.List as L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Data.Template.Model
import Data.Template.TH
import Debug.Trace

data RuntimeImport = Add String
                   | Qualify String String Bool
                   | Hide String
                   deriving (Eq, Show, Data)

-- | Get an expression of a list holding names of modules current module imports.
--
-- Splice the result of this function to make thied argument of @renderRuntime@.
currentModules :: ExpQ -- ^ Expression of the list.
currentModules = thisModule >>= importedModules

-- | Get an expression of a list holding names of modules imported by a module.
importedModules :: Module -- ^ Module.
                -> ExpQ -- ^ Expression of the list.
importedModules m = do
    ModuleInfo mods <- reifyModule m

    runtimes <- reifyAnnotations (AnnLookupModule m) :: Q [RuntimeImport]
    let moduleNames = catMaybes $ map (isAvailable $ hideModules runtimes) mods

    listE $ map (\m -> [| m |]) moduleNames
    where
        hideModules :: [RuntimeImport] -> [String]
        hideModules [] = []
        hideModules (Hide m:ms) = m : hideModules ms
        hideModules (m:ms) = hideModules ms

        isAvailable :: [String] -> Module -> Maybe String
        isAvailable hides (Module (PkgName p) (ModName m)) = if p /= "main" && not (m `elem` hides) then Just m else Nothing

-- | Render a template in runtime.
--
-- This function compile template into an expression of a function which takes an argument.
-- The tuple of 4th argument contains its value, name (variable name used in the template) and type.
--
-- TODO
-- Currently, any special import (qualified, hiding, etc) is completely ignored and all listed modules are imported.
-- This can cause the compilation error by conflict of names or some other reasons.
renderRuntime :: forall a. (Typeable a)
              => RenderContext -- ^ Rendering context.
              -> FilePath -- ^ File path of the template.
              -> [String] -- ^ Module names needed to compile the template.
              -> (a, String, Type) -- ^ Argument and its name and type to give compiled function.
              -> IORef (Maybe (String, UTCTime, Exp)) -- ^ Cache of the expression of the template.
              -> IO String -- ^ Rendered string.
renderRuntime context path mods (arg, key, t) cache = runQ $ do -- Q
    exp <- renderDynamicWithContext context path cache
    runIO $ runGhc (Just libdir) $ do -- GhcMonad
        --defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        withCleanupSession $ do
            dynFlags <- getSessionDynFlags
            setSessionDynFlags (foldl xopt_set dynFlags [DataKinds, TypeOperators, FlexibleContexts, OverloadedLabels, ScopedTypeVariables])
            let mods' = L.union mods ["Data.Template"]
            setContext $ (map (IIDecl . simpleImportDecl . mkModuleName) mods')
            let f = LamE [SigP (VarP $ mkName key) t] exp
            d <- dynCompileExpr $ (show $ ppr f)
            return $ (fromJust $ fromDynamic @(a -> String) d) arg

-- | Get the expression of the template with empty context.
renderDynamic :: FilePath -- ^ File path of the template.
              -> IORef (Maybe (String, UTCTime, Exp)) -- ^ Cache of the expression of the template.
              -> ExpQ -- ^ Rendered expression.
renderDynamic path cache = fst <$> renderDynamic' newContext path cache

-- | Get the expression of the template.
--
-- Third argument may have the cache of the expression with its rendered time.
-- When it does not exist or the rendered time is earlier than the last modified time of the file,
-- the contents of the template is loaded and evaluated.
-- Otherwise, the expression in the cache is returned.
renderDynamicWithContext :: RenderContext -- ^ Rendering context.
                         -> FilePath -- ^ File path of the template.
                         -> IORef (Maybe (String, UTCTime, Exp)) -- ^ Cache of the expression of the template.
                         -> ExpQ -- ^ Rendered expression.
renderDynamicWithContext context path cache = fst <$> renderDynamic' context path cache

renderDynamic' :: RenderContext
               -> FilePath
               -> IORef (Maybe (String, UTCTime, Exp))
               -> Q (Exp, Bool)
renderDynamic' context path cache = do
    c <- runIO $ readIORef cache
    modifiedAt <- runIO $ getModificationTime path

    case c of
        Just (n, t, exp) -> do
            if modifiedAt <= t then return (exp, False)
                               else reloadExp context path cache modifiedAt
        Nothing -> reloadExp context path cache modifiedAt
    where
        reloadExp :: RenderContext
                  -> FilePath
                  -> IORef (Maybe (String, UTCTime, Exp))
                  -> UTCTime
                  -> Q (Exp, Bool)
        reloadExp context path cache modifiedAt = do
            exp <- renderWithContext context path
            c <- runIO $ readIORef cache
            runIO $ writeIORef cache (Just (path, modifiedAt, exp)) >> return (exp, True)