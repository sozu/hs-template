{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Template.Model where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (maybe)
import Control.Applicative
import Control.Monad
import Language.Haskell.TH
import System.FilePath
import Debug.Trace

-- ------------------------------------------------------------
-- Templates.
-- ------------------------------------------------------------

{- | Type synonym representing a template structure.
-}
type Template = ([Directive], [TemplateToken])

{- | Get an extended template of a template if any.
-}
getParent :: Template -- ^ A template.
          -> Maybe FilePath -- ^ File path of an extended template if any.
getParent ([], _) = Nothing
getParent (Extends f:_, _) = Just f
getParent (x:xs, ts) = getParent (xs, ts)

-- ------------------------------------------------------------
-- Directives.
-- ------------------------------------------------------------

{- | Template Directives.

Directive is used to declare an information of the template.
Each directive occupies a line and it must be written in the top of a template file, that is, before any content.
It is enclosed by "{ - #" and "# - }" and the first word inside denotes the type of the directive.
Succeeding texts are arguments given to the directive.
Unlike template tags, any Haskell expression is not available inside the directive.

- extends [file]
    Extends directive indicates that the template inherits another template [file].
    If a template has this directive, global contents outside block or import tags will ignored completely.
    Otherwise, they are treated as contents to be rendered.

- encoding [encoding]
    Encoding directive indicates the encoding of the template.
    TODO:
    This directive does not work yet.

-}
data Directive = Extends String
               | Encoding String
               deriving (Show, Eq)

-- ------------------------------------------------------------
-- Tags.
-- ------------------------------------------------------------

{- | Token types.

A template is first separated to tokens and then reconstructed into hierarchy form.
-}
data TemplateToken = Raw String -- ^ String token which will be rendered as it is.
                   | TagBegin Tag -- ^ Tag begining its scope.
                   | TagEnd -- ^ Tag ending its scope.
                   | TagAutonomous Tag -- ^ Autonomous tag which does not have any child token.
                   | Comment -- ^ Comment.
                   deriving (Show, Eq)

{- | Template Tags.

A tag is embeded to contents and controls the rendering result of them.
By default, every tag is enclosed by "{ -" and "- }" just like Haskell comment.
Inside a tag, the first character specified its type, for example, a tag beginning with "{ - @" is a block tag.
Each tag requires arguments in its own forms as described in the definition of tags below.

There is one more syntax about tags which associates with the tag scope.
Some tags create their own scope and they needs closing tag ("{ - $ - }") to close the scope.
Other tags don't need the closing tag and don't create any scope.
You can also close the scope by using "$ - }" instead of "- }" at the end of tag.
This represents that there are no contents in its scope.
Examples are:
- {-@ foo -}abc{-$-} : Block tag whose name is "foo" and "abc" is the content of its scope.
- {-@ foo $-} : Block tag whose name is "foo" and has no contents.
- {-< bar.html -} : Include tag for "bar.html". This tag doesn't create a scope and closing tag is not required.
- {-< bar.html $-} : The same as above, '$' has no effect for no-scope tag.

Descriptions of no-scope tags:
  
- [expression]
    In case no tag specifying character exists, string in the tag is considered as Haskell expression and its evaluation is inserted.
- & [file]
    Import tag which imports macros defined it [file]. The macros are available after this tag.
    This tag does not render any contents.
- < [file]
    Include tag which inserts the contents of [file].
    The contents are interpreted under current context, thus tags written in [file] is valid also.
- = [name] (arg1 expresstion) ... (argN expression)
    Call tag which calls a macro of [name] with given arguments and inserts the result.
    The macro must be declared somewhere in template hierarchy or imported beforehand.
    TODO: Because arguments must be separated with spaces, expression including spaces is not available completely.

Descriptions of scope tags:

- | [pattern] = [expression]
    Definition tag which defines variables. [pattern] is a pattern of Haskell syntax like "x", "(x, y)", "xs@(x:_)".
    You should be careful about the scope of the variables.
    If this tag has corresponding closing tag, they are available only in that scope.
    Otherwise, that is, when this tag ends with "$ - }", variabless are available after this tag until the parent scope closes.
    This specific scope is to avoid the flood of closing tag in the template.
- @ [name]
    Block tag which defines a block of contents with the given [name].
    In the root template, this tag also decides the position where the contents will be inserted.
    A block in inherited template overrides the contents of the block of the same [name] written in parent template.
- % [index] [value] [list expression]
    Loop tag which repeats contents in its scope. The number of loops is the same as the length of [list expression].
    In the contents, two variables named as [index] and [value] are available.
    [index] denotes current loop index (Int) starting with 0 and [value] denotes the value at [index] of the list.
- ? [bool expression] / !? [bool expression] / !
    If tag which contains several scopes and inserts the first one whose [bool expression] is True.
    When all [bool expression] of ? and !? are False, the contents in ! scope is inserted if it exists.
    The behavior of this tag is the same as if ~ else if ~ else structure in many programming languages.
    You must start with ? tag, can write !? tags as many as you like and may use ! tag if necessary.
    Closing tag must appear just once at the end of all scopes, for example, next to the scope of ! tag.
- * [name] [arg1] ... [argN]
    Macro tag which defines its contents as the macro named as [name].
    A macro accepts several arguments and each of them is available as a variable whose name is [argX].
    This tag just defines a macro and inserts no contents.
-}
data Tag = Def String String
         | If String
         | Else
         | ElseIf String
         | For String String (Maybe String)
         | Import String
         | Includes String
         | Block String
         | Eval String
         | Macro String [String]
         | Call String [String]
         | End
         | Eof
         deriving (Show, Eq)

noScope :: Tag
        -> Bool
noScope (Import _) = True
noScope (Includes _) = True
noScope (Eval _) = True
noScope (Call _ _) = True
noScope _ = False

-- ------------------------------------------------------------
-- Nodes.
-- ------------------------------------------------------------

{- | Types of nodes of tree structure.
-}
data TagNode = TagNode Tag [TagNode]
             | IfNode [(String, [TagNode])] (Maybe [TagNode])
             | RawNode String
             deriving (Show, Eq)

{- | Convert a list of tokens into tree structure.
-}
makeHierarchy :: [TemplateToken] -- ^ A list of tokens.
              -> [TagNode] -- ^ Root nodes of tree representing a template.
makeHierarchy [] = []
makeHierarchy ts = let (n, ts') = makeNode ts in maybe (makeHierarchy ts') (: (makeHierarchy ts')) n

append :: [a] -> a -> [a]
append xs x = reverse $ x : reverse xs

{- | Create a node by consuming tokens from the top of token list.
-}
makeNode :: [TemplateToken] -- ^ A list of tokens.
         -> (Maybe TagNode, [TemplateToken]) -- ^ A created node if exists and remaining tokens.
makeNode (Raw s : ts) = (Just $ RawNode s, ts)
makeNode (Comment : ts) = (Nothing, ts)
makeNode (TagAutonomous t@(Def _ _) : ts) = adds (TagNode t []) ts
    where
        add (TagNode t ns) ts = let (n, ts') = makeNode ts in (TagNode t (maybe ns (append ns) n), ts')
        adds n [] = (Just n, []) -- Global definition reaches the end of input without TagEnd.
        adds n ts@(TagEnd : _) = (Just n, ts) -- Do not consume TagEnd which ends parent scope.
        adds n ts@(TagBegin (ElseIf _) : _) = (Just n, ts)
        adds n ts@(TagBegin Else : _) = (Just n, ts)
        adds n ts = let (n', ts') = add n ts in adds n' ts'
makeNode (TagAutonomous t : ts) = (Just $ TagNode t [], ts)
makeNode (TagBegin (If s) : ts) = adds (IfNode [(s, [])] Nothing) ts
    where 
        add :: TagNode -> [TemplateToken] -> (TagNode, [TemplateToken])
        add (IfNode ns e) (TagBegin (ElseIf s) : ts) = (IfNode (append ns (s, [])) e, ts)
        add (IfNode ns e) (TagBegin Else : ts) = (IfNode ns (Just []), ts)
        add nn@(IfNode ns Nothing) ts =
            let (n, ts') = makeNode ts
                (c, ns') = last ns
            in (maybe nn (\n' -> IfNode (append (init ns) (c, append ns' n')) Nothing) n, ts')
        add (IfNode ns (Just es)) ts = let (n, ts') = makeNode ts in (IfNode ns (Just $ maybe es (append es) n), ts')
        adds :: TagNode -> [TemplateToken] -> (Maybe TagNode, [TemplateToken])
        adds n (TagEnd : ts) = (Just n, ts)
        adds n ts = let (n', ts') = add n ts in adds n' ts'
makeNode (TagBegin t : ts)
    | noScope t = (Just $ TagNode t [], ts)
    | otherwise = adds (TagNode t []) ts
    where
        add (TagNode t ns) ts = let (n, ts') = makeNode ts in (TagNode t (maybe ns (append ns) n), ts')
        adds n (TagEnd : ts) = (Just n, ts)
        adds n ts = let (n', ts') = add n ts in adds n' ts'

-- ------------------------------------------------------------
-- Contexts.
-- ------------------------------------------------------------

{- | Contextual values used to render template.
-}
data RenderContext = RenderContext { variables :: M.Map String (Q Exp) -- ^ Variables where keys are their names.
                                   , blocks :: M.Map String [TagNode] -- ^ Blocks distinguished by their names.
                                   , macros :: M.Map String ([String], [TagNode]) -- ^ Macros distinguished by their names.
                                   , rootPath :: FilePath
                                   }

{- | Creates a empty context.
-}
newContext :: RenderContext
newContext = RenderContext M.empty M.empty M.empty "."

{- | Puts a variable into the context.
-}
(+=) :: RenderContext -- ^ Rendering context.
     -> (String, ExpQ) -- ^ Name and expression of the variable.
     -> RenderContext -- ^ Modified context.
(+=) c (k, v) = c { variables = M.insert k v (variables c) }

{- | Puts a variable into the context if its name exists.
-}
(?+=) :: RenderContext -- ^ Rendering context.
      -> (Maybe String, ExpQ) -- ^ Name and expression of the variable. If name is `Nothing`, this function does nothing.
      -> RenderContext -- ^ Modified context.
(?+=) c (k, v) = maybe c (\n -> c += (n, v)) k

{- | Puts a block into the context.
-}
(+@~) :: RenderContext -- ^ Rendering context.
      -> (String, [TagNode]) -- ^ Name and nodes of the block.
      -> RenderContext -- ^ Modified context.
(+@~) c (n, ts) = let bs = blocks c in c { blocks = M.alter (\v -> v <|> Just ts) n bs }

{- | Puts a macro into the context.
-}
(+&~) :: RenderContext -- ^ Rendering context.
      -> (String, [String], [TagNode]) -- ^ Name, name of arguments and nodes of the macro.
      -> RenderContext -- ^ Modified context.
(+&~) c (n, as, ts) = let ms = macros c in c { macros = M.alter (\v -> v <|> Just (as, ts)) n ms }

{- | Create file path under given context.

If given path is absolute, it is returned as it is.
Otherwise, new path is created by joining it under the contextual root path.
-}
resolvePath :: RenderContext -- ^ Rendering context.
            -> FilePath -- ^ A path to resolve by joined to contextual root path.
            -> FilePath -- ^ Resolved path.
resolvePath c path
    | isAbsolute path = path
    | otherwise = normalise $ rootPath c </> path

{-| Update contextual root path.
-}
setRoot :: RenderContext -- ^ Rendering context.
        -> FilePath -- ^ New root path.
        -> RenderContext -- ^ Modified context.
setRoot c path = c { rootPath = takeDirectory path }

{- | Creates new expression by adding contextual variables into its scope.
-}
letC :: RenderContext -- ^ Rendering context.
     -> ExpQ -- ^ An expression.
     -> ExpQ -- ^ Expression including contextual variables in its scope.
letC c = \e -> do
    let decs = map dq $ M.toList (variables c)
    letE decs e
    where
        dq (k, v) = valD (varP (mkName k)) (normalB v) [] -- DecQ

eval :: RenderContext
     -> String
     -> Q Exp
eval c n = maybe (varE $ mkName n) id (M.lookup n $ variables c)

-- ------------------------------------------------------------
-- Representations.
-- ------------------------------------------------------------

{- | A class which declares a method to convert an instance value to a string.

Any expression in Eval tag is rendered by using this method.
-}
class Repr r where
    {- | Convert a value to a string.
    -}
    repr :: r -- ^ A value.
         -> String -- ^ Converted string.

instance {-# OVERLAPPABLE #-} (Show r) => Repr r where
    repr = show

instance {-# OVERLAPPING #-} Repr [Char] where
    repr s = s
