{-# LANGUAGE MultiWayIf #-}

module Data.Template.Parser where

import Control.Monad
import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe (maybe, fromJust, isNothing, isJust)
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Data.Template.Model
import Debug.Trace

hs = makeTokenParser haskellDef

-- ------------------------------------------------------------
-- Constants.
-- ------------------------------------------------------------

beginTag :: String
beginTag = "{-"

endTag :: String
endTag = "-}"

beginDirective :: String
beginDirective = "{-#"

endDirective :: String
endDirective = "#-}"

closeSymbol :: Char
closeSymbol = '$'

-- | Parses template string and returns the result.
parseTemplate :: String -- ^ Template string.
              -> Either ParseError Template -- ^ Direvtives and tokens if the template is valid, otherwise ParseError.
parseTemplate template = parse parser "" template
    where
        parser = do
            ds <- many (try parseDirective) <|> return []
            tokens <- parseRaw
            return (ds, removeTagLine tokens)

-- ------------------------------------------------------------
-- Directive parsers.
-- ------------------------------------------------------------

{- | Parses a directive.
-}
parseDirective :: Parsec String u Directive
parseDirective = do
    d <- try lineD
    case parse ((choice $ map try dps) <?> "Unrecognized directive") "" d of
        Left e -> fail (show e)
        Right r -> return r
    where
        dps = [extendsD, encodingD]

lineD :: Parsec String u String
lineD = string beginDirective *> spaces *> manyTill anyChar (try $ string endDirective) <* manyTill anyChar endOfLine

extendsD :: Parsec String u Directive
extendsD = Extends <$> (string "extends" *> many1 space *> manyTill anyChar (try space) <* spaces)

encodingD :: Parsec String u Directive
encodingD = Encoding <$> (string "encoding" *> many1 space *> manyTill anyChar (try space) <* spaces)

-- ------------------------------------------------------------
-- Token parsers.
-- ------------------------------------------------------------

{- | Parses template string after directives and construct a list of tokens.
-}
parseRaw :: Parsec String u [TemplateToken]
parseRaw = do
    ds <- (many $ try parseDirective) <|> return []
    s <- try (manyTill anyChar (lookAhead $ string beginTag)) <|> many anyChar
    let raw = if s == "" then id else (Raw s :)
    next <- eof *> return (Eof, False) <|> parseTag
    case next of
        (Eof, _) -> return $ raw []
        (End, _) -> parseRaw >>= \ts -> return $ raw (TagEnd : ts)
        (tag, a) -> parseRaw >>= \ts -> return $ raw ((if a then TagAutonomous else TagBegin) tag : ts)

removeTagLine :: [TemplateToken]
              -> [TemplateToken]
removeTagLine ts = let (tokens, line) = foldl accumulateLine ([], []) ts in tokens ++ reverse line

isLineBreak :: Char
            -> Bool
isLineBreak c = c == '\r' || c == '\n'

{- | Splits a string with the first line break characters.

    > let str = "abc\r\ndef\r\nghi"
    > splitFirstLine str -- ("abc\r\n", "def\r\nghi", True)
-}
splitFirstLine :: String -- ^ A string to split.
               -> (String, String, Bool) -- ^ String before and including line breaks, subsequent string and whether the original string has line breaks.
splitFirstLine [] = ("", "", False)
splitFirstLine ('\r':'\n':s) = ("\r\n", s, True)
splitFirstLine ('\r':s) = ("\r", s, True)
splitFirstLine ('\n':s) = ("\n", s, True)
splitFirstLine (c:s) = let (x, y, r) = splitFirstLine s in (c:x, y, r)

{- | Splits a string with the last line break characters.

    > let str = "abc\r\ndef\r\nghi"
    > splitLastLine str -- ("abc\r\ndef\r\n", "ghi")
-}
splitLastLine :: String -- ^ A string to split.
              -> (String, String) -- ^ String before and including line breaks and subsequent string.
splitLastLine s =
    let index = L.findIndex isLineBreak (reverse s)
    in maybe ("", s) (\i -> splitAt (length s - i) s) index

{- | Accumulates tokens which complete lines with line break character.

    Until Raw token including line break comes, accepted tokens remain unaccumulated.
    This function is designed to be a folding function for token list.
-}
accumulateLine :: ([TemplateToken], [TemplateToken]) -- ^ Accumulated and unaccumulated tokens.
               -> TemplateToken -- ^ New token.
               -> ([TemplateToken], [TemplateToken]) -- ^ Modified accumulation state.
accumulateLine (acc, []) t = (acc, [t])
accumulateLine (acc, line) t@(Raw s) =
    let (seg, after, has) = splitFirstLine s
    in if has
        then let (acc', line') = truncateLine line (s, seg, after) in (acc ++ acc', line')
        else (acc, t:line) -- Line is not completed. Add this token in reversed order.
accumulateLine (acc, line) t = (acc, t:line)

{- | Separates tokens into tokens constructing lines ended with line breaks and remaining tokens which don't end a line.

    Given tokens are tokens which have not been accumulated because they can possibly be truncated.
    Second argument brings the information of next Raw token (all characters, the first line, remaining characters).
    If the last line of the tokens has only truncatable characters (space or tag) or tags generating empty string
    and the first line of the next Raw token has also only truncatable characters, those chracters are removed.

    > let (acc, rem) = truncateLine [Raw "abc\r\ndef\r\n  \t  ", empty tags..., Raw "  \t  \r\nghi"]
    > -- acc == [Raw "abc\r\rdef\r\n", empty tags...]
    > -- rem == [Raw "ghi"]
-}
truncateLine :: [TemplateToken] -- ^ Unaccumulated tokens.
             -> (String, String, String) -- ^ The information of next Raw token.
             -> ([TemplateToken], [TemplateToken]) -- ^ Tokens to be accumulated and tokens which begin next line.
truncateLine line (s, seg, after) =
    let (truncatable, first) = checkFirst (last line)
    in if truncatable && all isIgnorable seg && all isSpaceToken (init line)
            then (maybe id (:) first $ reverse (init line), if after == "" then [] else [Raw after])
            else if after == ""
                    then (reverse (Raw s:line), []) -- ^ This token ends with line break.
                    else (reverse line, [Raw s]) -- ^ This token has next line segment.
    where
        isIgnorable :: Char -> Bool
        isIgnorable c = c == ' ' || c == '\t' || isLineBreak c

        -- Checks whether all characters in its last line are ignorable.
        checkFirst :: TemplateToken -> (Bool, Maybe TemplateToken)
        checkFirst (Raw s) = let (before, seg) = splitLastLine s
                             in if all isIgnorable seg
                                    then (True, if before == "" then Nothing else Just (Raw before))
                                    else (False, Nothing)
        checkFirst t = if isSpaceToken t then (True, Just t) else (False, Nothing)

        -- Checks whether a token will be rendered into empty string.
        isSpaceToken :: TemplateToken -> Bool
        isSpaceToken (Raw s) = False
        isSpaceToken (TagBegin (Eval _)) = False
        isSpaceToken (TagAutonomous (Eval _)) = False
        isSpaceToken (TagBegin (Includes _)) = False
        isSpaceToken (TagAutonomous (Includes _)) = False
        isSpaceToken _ = True

-- | Removes leading spaces from a string.
lstrip :: String -- ^ A string.
       -> String -- ^ String without leading spaces.
lstrip s@(c:cs)
    | C.isSpace c = cs
    | otherwise = s

-- | Removes leading and trailing spaces from a string.
strip :: String -- ^ A string.
      -> String -- ^ String without leading and trailing spaces.
strip = reverse . lstrip . reverse . lstrip

{- | Parses a tag and returns it and whether it is autonomous.
-}
parseTag :: Parsec String u (Tag, Bool)
parseTag = do
    string beginTag
    s <- manyTill anyChar (try $ string endTag)
    let (inner, autonomous) = if last s == closeSymbol && strip s /= "$" then (init s, True) else (s, False)
    case parse ((choice $ map try tps) <?> "Unrecognized tag") "" inner of
        Left e -> fail (show e)
        Right r -> return (r, autonomous)
    where
        tps = [evalT, defT, importT, includesT, blockT, forT, ifT, elseifT, elseT, macroT, callT, endT]

evalT :: Parsec String u Tag
evalT = Eval <$> (char ' ' *> spaces *> many1 anyChar)

defT :: Parsec String u Tag
defT = Def <$> (char '|' *> many1 space *> (lexeme hs) (identifier hs) <* char '=') <*> (spaces *> many1 anyChar)

importT :: Parsec String u Tag
importT = Import <$> (char '&' *> many1 space *> many1 (satisfy $ not . C.isSpace) <* spaces <* eof) 

includesT :: Parsec String u Tag
includesT = Includes <$> (char '<' *> many1 space *> many1 (satisfy $ not . C.isSpace) <* spaces <* eof)

blockT :: Parsec String u Tag
blockT = Block <$> (char '@' *> many1 space *> (lexeme hs) (identifier hs))

forT :: Parsec String u Tag
forT = mkFor <$> (char '%' *> spaces *> (lexeme hs) (identifier hs)) <*> (lexeme hs) (identifier hs) <*> (many1 anyChar)
    where
        mkFor i v expr = For v expr (Just i)

ifT :: Parsec String u Tag
ifT = If <$> (char '?' *> many1 space *> many1 anyChar)

elseifT :: Parsec String u Tag
elseifT = ElseIf <$> (string "!?" *> many1 space *> many1 anyChar)

elseT :: Parsec String u Tag
elseT = string "!" *> return Else

macroT :: Parsec String u Tag
macroT = Macro <$> (char '*' *> many1 space *> many1 (try $ satisfy $ not . C.isSpace)) <*> (spaces *> many ((lexeme hs) (identifier hs)))

callT :: Parsec String u Tag
callT = Call <$> (char '=' *> many1 space *> many1 (try $ satisfy $ not . C.isSpace)) <*> (spaces *> many (many1 (try $ satisfy $ not . C.isSpace) <* spaces))

endT :: Parsec String u Tag
endT = spaces *> string "$" *> return End

-- ------------------------------------------------------------
-- Functions to interpret template into the form of Template Haskell expression.
-- ------------------------------------------------------------

-- TODO
-- If there is a recursive inheritance or file inclusion/import,
-- the analysis of template will get in infinite loop.

{- | Reads and parses a template file.
-}
parseTemplateFile :: FilePath -- ^ Template file path.
                  -> IO Template -- ^ Parsed template.
parseTemplateFile path = do
    contents <- BS.readFile path
    case parseTemplate (T.unpack $ decodeUtf8 contents) of
        Left e -> fail (show e)
        Right t -> return t

{- | Extracts definition nodes in a template and apply them to the context.
-}
resolveContext :: RenderContext -- ^ Rendering context.
               -> Bool -- ^ Whether to add block contents or not.
               -> IO Template -- ^ Template.
               -> IO RenderContext -- ^ Modified context.
resolveContext c resolveBlock template = do
    t <- template
    let accum = \cxt node -> case node of
                    TagNode (Block n) ts -> return $ if resolveBlock then cxt +@~ (n, ts) else cxt
                    TagNode (Macro n as) ts -> return $ cxt +&~ (n, as, ts)
                    TagNode (Import f) _ -> resolveContext cxt False (parseTemplateFile (resolvePath c f))
                    _ -> return cxt
    foldM accum c (makeHierarchy $ snd t)

