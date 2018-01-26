{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module declares many parsers for Haskell syntax.

    Referred specification is https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-220003

    TODO:
    Some kinds of expressions are not parseable, for example application of data constructor.
-}
module Data.Template.Expr where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Proxy
import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token
import Language.Haskell.TH
import qualified Control.Exception.Safe as E

-- Most of parsers depend on parsers defined in Text.Parsec.Language and Text.Parsec.Token.
-- Memo:
-- > reservedOpNames haskellDef
-- ["::","..","=","\\","|","<-","->","@","~","=>"]
--
-- > reservedNames haskellDef                                                                                                                                                                                                                   
-- ["let","in","case","of","if","then","else","data","type","class","default","deriving","do"
-- ,"import","infix","infixl","infixr","instance","module","newtype","where","primitive","foreign"
-- ,"import","export","primitive","_ccall_","_casm_","frall"]

-- ------------------------------------------------------------
-- Public functions.
-- ------------------------------------------------------------

-- | Get an expression represented by a given string.
-- When the string is not a valid Haskell expression, exception is thrown.
getExpr :: String -- ^ A string representing an expression.
        -> ExpQ -- ^ Interpreted expression.
getExpr s = case parse infixexp "" s of
                Left e -> E.throw $ TemplateExprException (show e)
                Right r -> r

-- | Get a type represented by a given string.
-- When the string is not a valid Haskell type, exception is thrown.
getType :: String
        -> TypeQ
getType s = case parse typ "" s of
                Left e -> E.throw $ TemplateExprException (show e)
                Right r -> r

-- | Exception thrown when the invalid string is given to getExpr or getType.
data TemplateExprException = TemplateExprException String deriving (Show)

instance E.Exception TemplateExprException where

hs = makeTokenParser haskellDef

-- ------------------------------------------------------------
-- Independent expressions.
-- ------------------------------------------------------------

-- | Parses qualifier and succeeding period.
qualifier :: Parsec String u String
qualifier = (:) <$> upper <*> many alphaNum <* char '.'

-- | Parses an operator and its qualifier if exists.
qop :: Parsec String u ExpQ
qop = do
    q <- optionMaybe qualifier
    op <- operator hs
    return $ varE (mkName $ maybe "" (\q' -> q' ++ ".") q ++ op)

-- | Parses a variable operator enclosed by "``".
qvarop :: Parsec String u ExpQ
qvarop = do
    (symbol hs) "`"
    q <- optionMaybe qualifier
    op <- identifier hs
    (symbol hs) "`"
    return $ varE (mkName $ maybe "" (\q' -> q' ++ ".") q ++ op)

-- | Parses an infix operator (qop | qvarop).
ope :: Parsec String u ExpQ
ope = try qop <|> qvarop

-- | Parses a variable and its qualifier if exists.
qvar :: Parsec String u ExpQ
qvar = do
    q <- optionMaybe qualifier
    op <- identifier hs
    return $ varE (mkName $ maybe "" (\q' -> q' ++ ".") q ++ op)

-- | Parses a literal expression.
litexp :: Parsec String u ExpQ
litexp = try (stringLiteral hs >>= return . litE . stringL)
     <|> try (charLiteral hs >>= return . litE . charL)
     <|> try (natural hs >>= return . litE . integerL)
     <|> (float hs >>= return . litE . doublePrimL . toRational)

{- | Parses overloaded label.

    template-haskell-2.12.0.0 has not supported OverloadedLabels and TypeApplications yet.
    This method returns an expression of the function to get a filed of extensible record as below.

    > view (itemAssoc (Proxy :: Proxy "label"))

    Threfore, importing Data.Extensible is necessary to render template including this expression.
-}
labelexp :: Parsec String u ExpQ
labelexp = do
    char '#'
    label <- identifier hs
    return $ appE (unboundVarE $ mkName "view")
                  (appE (unboundVarE $ mkName "itemAssoc")
                        (sigE (conE 'Proxy) (appT (conT ''Proxy) (litT $ strTyLit label))))

-- ------------------------------------------------------------
-- Recursive expressions.
-- ------------------------------------------------------------

-- | Parses a negative number which begins with '-' (- infixexp).
minus :: Parsec String u ExpQ
minus = do
    (symbol hs) "-"
    v <- infixexp
    return $ appE [| negate |] v

-- | Parses an inside of tuple expression (infixexp1, ..., infixexpN).
tupleexp :: Parsec String u ExpQ
tupleexp = do
    vs <- (commaSep hs) infixexp
    return $ tupE vs

-- | Parses an inside of list expression (infixexp1, ..., infixexpN).
listexp :: Parsec String u ExpQ
listexp = do
    vs <- (commaSep hs) infixexp
    return $ listE vs

-- | Parses a left section ((infixexp ope)).
lsec :: Parsec String u ExpQ
lsec = do
    let plsec = sequence [infixexp, ope]
    vop <- (parens hs) plsec
    return $ infixE (Just $ vop !! 0) (vop !! 1) Nothing

-- | Parses a right section (((ope) infixexp)).
rsec :: Parsec String u ExpQ
rsec = do
    let prsec = sequence [(parens hs) ope, infixexp]
    opv <- (parens hs) prsec
    return $ infixE Nothing (opv !! 0) (Just $ opv !! 1)

-- | Parses a lambda expression (\apat -> infixexp).
lambda :: Parsec String u ExpQ
lambda = do
    (symbol hs) "\\"
    vs <- (commaSep hs) apat
    (symbol hs) "->"
    e <- infixexp
    return $ lamE vs e

-- | Parses a "let" expression (let decl in infixexp).
plet :: Parsec String u ExpQ
plet = do
    (symbol hs) "let"
    ds <- (commaSep1 hs) decl
    (symbol hs) "in"
    e <- infixexp
    return $ letE ds e

-- | Parses an "if" expression (if infiexp [;] then infixexp [;] else infixexp).
pif :: Parsec String u ExpQ
pif = do
    (symbol hs) "if"
    pred <- infixexp
    optional (semi hs)
    (symbol hs) "then"
    et <- infixexp
    optional (semi hs)
    (symbol hs) "else"
    ef <- infixexp
    return $ condE pred et ef

-- | Parses a "case" expression (case infixexp of alt1; ...; altN).
pcase :: Parsec String u ExpQ
pcase = do
    (symbol hs) "case"
    e <- infixexp
    (symbol hs) "of"
    ms <- (semiSep1 hs) alt
    return $ caseE e ms

-- | Parses an expression.
aexp :: Parsec String u ExpQ
aexp = try qvar
   <|> try litexp
   <|> try labelexp
   <|> try ((parens hs) infixexp)
   <|> try ((parens hs) tupleexp)
   <|> try ((brackets hs) listexp)
   <|> try lsec
   <|> rsec

-- | Parses an application of a function.
fexp :: Parsec String u ExpQ
fexp = do
    -- The specification (fexp -> [fexp] aexp) causes left recursion.
    -- Just repeating expressions is enough?
    as <- many1 aexp
    return $ app as
    where
        app :: [ExpQ] -> ExpQ
        app (a:[]) = a
        app (f:a:fs) = app ((appE f a) : fs)

-- | Parses language specific expression.
lexp :: Parsec String u ExpQ
lexp = try lambda
   <|> try plet
   <|> try pif
   <|> try pcase
   <|> fexp

-- | This is (currently) a complete expression parser of this module.
infixexp :: Parsec String u ExpQ
infixexp = try (infixE <$> (lexp >>= return . Just) <*> ope <*> (infixexp >>= return . Just))
       <|> try minus
       <|> lexp

-- ------------------------------------------------------------
-- Independent patterns.
-- ------------------------------------------------------------

-- | Parses a wild card pattern (_).
wcp :: Parsec String u PatQ
wcp = (symbol hs) "_" *> return wildP

-- | Parses a variable pattern.
varp :: Parsec String u PatQ
varp = identifier hs >>= return . varP . mkName

-- | Parses a literal pattern.
litp :: Parsec String u PatQ
litp = try (stringLiteral hs >>= return . litP . stringL)
   <|> try (charLiteral hs >>= return . litP . charL)
   <|> try (integer hs >>= return . litP . integerL)
   <|> (float hs >>= return . litP . doublePrimL . toRational)

-- ------------------------------------------------------------
-- Recursive patterns.
-- ------------------------------------------------------------

-- | Parses an inside of tuple pattern (pat1, ..., patN).
tuplep :: Parsec String u PatQ
tuplep = do
    vs <- (commaSep hs) pat
    return $ tupP vs

-- | Parses an inside of list pattern (pat1, ..., patN).
listp :: Parsec String u PatQ
listp = do
    vs <- (commaSep hs) pat
    return $ listP vs

-- | Parses an "as" pattern (identifier@pat).
asp :: Parsec String u PatQ
asp = do
    v <- identifier hs <* string "@"
    p <- apat
    return $ asP (mkName v) p

-- | Parses a "cons" pattern (lpat:pat).
consp :: Parsec String u PatQ
consp = do
    l <- lpat
    (symbol hs) ":"
    r <- pat
    return $ infixP l '(:) r

-- | Parses a pattern.
apat :: Parsec String u PatQ
apat = try asp
   <|> try varp
   <|> try litp
   <|> try wcp
   <|> try ((parens hs) pat)
   <|> try ((parens hs) tuplep)
   <|> (brackets hs) listp

-- | Parses language specific pattern.
lpat :: Parsec String u PatQ
lpat = apat

-- | This is (currently) a complete pattern parser of this module.
pat :: Parsec String u PatQ
pat = try consp
  <|> lpat

-- ------------------------------------------------------------
-- Independent types.
-- ------------------------------------------------------------

-- | Parse a type variable.
tyvar :: Parsec String u TypeQ
tyvar = do
    try $ lookAhead lower
    v <- identifier hs
    return $ varT (mkName v)

-- | Parse a type constructor.
gtycon :: Parsec String u TypeQ
gtycon = do
    q <- optionMaybe $ try qualifier
    try $ lookAhead upper
    con <- identifier hs
    return $ conT (mkName $ maybe "" (\q' -> q' ++ ".") q ++ con)

-- ------------------------------------------------------------
-- Recursive types.
-- ------------------------------------------------------------

-- | Parses a tuple type ((typ1, ..., typN)).
tupt :: Parsec String u TypeQ
tupt = do
    ts <- ((parens hs) ((commaSep1 hs) typ))
    return $ app ((tupleT $ length ts) : ts)
    where
        app :: [TypeQ] -> TypeQ
        app (a:[]) = a
        app (a:b:ts) = app ((appT a b) : ts)

-- | Parses a list type ([typ]).
listt :: Parsec String u TypeQ
listt = do
    t <- (brackets hs) typ
    return $ appT listT t 

-- | Parses a sequence of types.
btype :: Parsec String u TypeQ
btype = do
    as <- many1 atype
    return $ app as
    where
        app :: [TypeQ] -> TypeQ
        app (a:[]) = a
        app (a:b:ts) = app ((appT a b) : ts)

-- | Parses a type.
atype :: Parsec String u TypeQ
atype = try gtycon
    <|> try tyvar
    <|> try tupt
    <|> try listt
    <|> ((parens hs) typ)

-- | This is (currently) a complete type parser of this module.
typ :: Parsec String u TypeQ
typ = btype

-- ------------------------------------------------------------
-- Other parsers.
-- ------------------------------------------------------------

-- | Parses a variable declaration (pat = infixexp).
decl :: Parsec String u DecQ
decl = do
    p <- pat
    (symbol hs) "="
    e <- infixexp
    return $ valD p (normalB e) []

-- | Parses a pattern matching in case expression (pat -> infixexp).
alt :: Parsec String u MatchQ
alt = do
    p <- pat
    (symbol hs) "->"
    e <- infixexp
    return $ match p (normalB e) []
