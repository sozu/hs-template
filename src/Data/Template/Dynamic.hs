{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Data.Template.Dynamic where

import System.Directory
import Control.Applicative
import Data.Dynamic
import Data.IORef
import Data.Time
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Data.Template.Model
import Data.Template.TH
import Debug.Trace

--compileDynamic :: FilePath
--               -> IO (Maybe String)
--compileDynamic path = runQ $ do
--    exp <- render path
--    runIO $ runGhc (Just libdir) $ do
--        getSessionDynFlags >>= setSessionDynFlags
--        dl <- parseImportDecl "import Data.Template.Model"
--        setContext [IIDecl dl]
--        d <- dynCompileExpr $ (show $ ppr exp)
--        return $ fromDynamic @String d
--
--outputSource :: FilePath
--             -> String
--             -> IO ()
--outputSource src content = return ()

renderDynamic :: FilePath
              -> IORef (Maybe (UTCTime, Exp))
              -> ExpQ
renderDynamic path cache = fst <$> renderDynamic' newContext path cache

renderDynamicWithContext :: RenderContext
                         -> FilePath
                         -> IORef (Maybe (UTCTime, Exp))
                         -> ExpQ
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