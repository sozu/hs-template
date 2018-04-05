{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Data.Template.Dynamic where

import GHC
import GHC.Paths
import DynFlags
import System.Directory
import Control.Applicative
import Control.Monad.IO.Class
import Data.Dynamic
import Data.IORef
import Data.Time
import Data.Maybe (fromJust)
import qualified Data.List as L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Data.Template.Model
import Data.Template.TH
import Debug.Trace

-- | Get an expression holding names of modules current module imports.
--
-- Splice the result of this function to make second argument of @renderRuntime@.
currentModules :: ExpQ -- ^ Expression holding names of modules current module imports.
currentModules = do
    this <- thisModule
    ModuleInfo mods <- reifyModule this
    listE $ map (\(p, m) -> [| m |]) $ filter ((/= "main") . fst) $ map (\(Module (PkgName p) (ModName m)) -> (p, m)) mods

-- | Render a template in runtime.
--
-- Module names MUST contain modules which directly define every functios used in the template.
-- In case @A.f@ is exported through @B@, module importing @B@ have to import @A@ as well
-- in order to render the template with this function.
-- Inside this function, @Data.Template.Model@ is added to the module list because it defines @repr@ used in most cases.
--
-- TODO
-- Currently, any special import (qualified, hiding, etc) is completely ignored and all listed modules are imported.
-- This can cause the compilation error if a name conflicts or some other reasons.
renderRuntime :: RenderContext -- ^ Rendering context.
              -> FilePath -- ^ File path of the template.
              -> [String] -- ^ Module names needed to compile the template.
              -> IORef (Maybe (UTCTime, Exp)) -- ^ Cache of the expression of the template.
              -> IO String -- ^ Rendered string.
renderRuntime context path mods cache = runQ $ do -- Q
    exp <- renderDynamicWithContext context path cache
    runIO $ runGhc (Just libdir) $ do -- GhcMonad
        defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
            dynFlags <- getSessionDynFlags
            setSessionDynFlags dynFlags
            let mods' = L.union mods ["Data.Template.Model"]
            setContext $ (map (IIDecl . simpleImportDecl . mkModuleName) mods')
            d <- dynCompileExpr $ (show $ ppr exp)
            return $ fromJust $ fromDynamic @String d

-- | Get the expression of the template with empty context.
renderDynamic :: FilePath -- ^ File path of the template.
              -> IORef (Maybe (UTCTime, Exp)) -- ^ Cache of the expression of the template.
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
                         -> IORef (Maybe (UTCTime, Exp)) -- ^ Cache of the expression of the template.
                         -> ExpQ -- ^ Rendered expression.
renderDynamicWithContext context path cache = fst <$> renderDynamic' context path cache

renderDynamic' :: RenderContext
               -> FilePath
               -> IORef (Maybe (UTCTime, Exp))
               -> Q (Exp, Bool)
renderDynamic' context path cache = do
    c <- runIO $ readIORef cache
    modifiedAt <- runIO $ getModificationTime path

    case c of
        Just (t, exp) -> do
            if modifiedAt <= t then return (exp, False)
                               else reloadExp context path cache modifiedAt
        Nothing -> reloadExp context path cache modifiedAt
    where
        reloadExp :: RenderContext
                  -> FilePath
                  -> IORef (Maybe (UTCTime, Exp))
                  -> UTCTime
                  -> Q (Exp, Bool)
        reloadExp context path cache modifiedAt = do
            exp <- renderWithContext context path
            c <- runIO $ readIORef cache
            runIO $ writeIORef cache (Just (modifiedAt, exp)) >> return (exp, True)