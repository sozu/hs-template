{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Data.Template.TH where

import qualified Data.Map as M
import Data.Maybe (maybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Template.Model
import Data.Template.Parser
import Data.Template.Expr
import Debug.Trace

-- ------------------------------------------------------------
-- Renderer.
-- ------------------------------------------------------------

-- | Render a template file in new context.
render :: FilePath -- ^ File path.
       -> ExpQ -- ^ Rendered expression.
render = execTemplateHierarchy generateExp newContext

-- | Render a template file in given context.
renderWithContext :: RenderContext -- ^ Rendering context.
                  -> FilePath -- ^ File path.
                  -> ExpQ -- ^ Rendered expression.
renderWithContext = execTemplateHierarchy generateExp

-- ------------------------------------------------------------
-- Utilities.
-- ------------------------------------------------------------

{- | Type synonym for functions to render node.

Returned expression represents a string of the node.
-}
type Generator = RenderContext -> TagNode -> ExpQ

{- | Generates an expression for a string representing a list of tag nodes.
-}
generates :: RenderContext -- ^ Rendering context.
          -> [TagNode] -- ^ A list of tag nodes.
          -> ExpQ -- ^ An expression for the representation of the nodes.
generates _ [] = [| "" |]
generates c ts = foldExp $ map (generateExp c) ts

{- | Merge expressions into an expression.
-}
foldExp :: [ExpQ] -- ^ Expressions to merge.
        -> ExpQ -- ^ Merged expression.
foldExp [] = [| "" |]
foldExp qs = foldl1 concat' qs
    where
        concat' :: ExpQ -> ExpQ -> ExpQ
        --concat' q1 q2 = [| repr $(q1) ++ repr $(q2) |]
        concat' q1 q2 = infixE (Just $ appE (varE $ mkName "repr") q1) 
                               (varE $ mkName "++")
                               (Just $ appE (varE $ mkName "repr") q2)

{- | Get an expression of the first value whose condition is true.
-}
selectIf :: [a] -- ^ Values to check.
         -> (a -> ExpQ) -- ^ A function to obtain a boolean expression from the value.
         -> (a -> ExpQ) -- ^ A function to obtain a representation from the value.
         -> ExpQ -- ^ An expression for the representation of the first true value.
selectIf (x:[]) _ g = g x
selectIf (x:xs) f g = [| if $(f x) then $(g x) else $(selectIf xs f g) |]

{- | Calls a macro and returns rendering result.
-}
callMacro :: RenderContext -- ^ Rendering context.
          -> String  -- ^ Macro name.
          -> [String] -- ^ Expressions of arguments.
          -> ExpQ -- ^ An expression for the representation of the macro.
callMacro c n args = maybe [| "" |] call (M.lookup n (macros c))
    where
        call (as, ts) = let c' = foldl (\cxt (a, v) -> cxt += (a, getExpr v)) c (zip as args)
                        in generates c' ts

{- | Generate Template Haskell expression froma a template.

This function resolves template inheritance and macro import if exists.
If the template is not a root template,
global tokens except for block/macro/import tag are discarded.
-}
execTemplateHierarchy :: Generator -- ^ Expression generation function.
                      -> RenderContext -- ^ Rendering context.
                      -> FilePath -- ^ File path of the template.
                      -> ExpQ -- ^ Generated expression.
execTemplateHierarchy g c path = do
    let path' = resolvePath c path
    template <- runIO $ parseTemplateFile path'
    c' <- runIO $ resolveContext (setRoot c path') True (return template)
    case getParent template of
        Nothing -> foldExp $ map (g c') (makeHierarchy $ snd template)
        Just f -> execTemplateHierarchy g c' f

-- ------------------------------------------------------------
-- Core generator.
-- ------------------------------------------------------------

{- | Generates an expression for a string representing  a tag node.
-}
generateExp :: RenderContext -- ^ Rendering context.
            -> TagNode -- ^ A tag node.
            -> ExpQ -- ^ An expression for the representation of the node.
generateExp _ (RawNode s) = [| s |]
generateExp c (TagNode (Eval s) ts) = [| $(varE $ mkName "repr") $(letC c (getExpr s)) |]
generateExp c (TagNode (Def k v) ts) = let c' = c += (k, getExpr v) in generates c' ts
generateExp c (TagNode (Includes f) ts) = execTemplateHierarchy generateExp c f
generateExp c (TagNode (Block n) ts) = maybe [| "" |] (generates c) (M.lookup n (blocks c))
generateExp c (TagNode (Call n args) ts) = callMacro c n args
generateExp c (TagNode (For v vs i) ts) = [|
        let values = $(letC c (getExpr vs))
            generate = \(v_, i_) -> $(generates (c += (v, varE 'v_) ?+= (i, varE 'i_)) ts)
        in $(varE $ mkName "foldl")
                $(varE $ mkName "++")
                ""
                -- Type signature is required to apply repr to literal number.
                ($(varE $ mkName "map") generate ($(varE $ mkName "zip") values ([0..] :: [$(conT $ mkName "Int")])))
    |]
generateExp c (IfNode ifs fallback) =
    let clauses = ifs ++ [maybe ("", []) ("", ) fallback]
    in selectIf clauses (\(cond, _) -> letC c (getExpr cond)) (\(_, ts) -> generates c ts)
generateExp _ _ = [| "" |]
