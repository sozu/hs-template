{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Template.Exp (
    getExp
    , getType
) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (maybe)
import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token
import Language.Haskell.TH
import qualified Control.Exception.Safe as E

data TemplateExpException = TemplateExprException String deriving (Show)

instance E.Exception TemplateExpException where

hs = makeTokenParser haskellDef

minus :: Parsec String u Exp
minus = do
    (symbol hs) "-"
    v <- infixexp
    return $ AppE (VarE 'negate) v

qualifier :: Parsec String u String
qualifier = (:) <$> upper <*> many alphaNum <* char '.'

qop :: Parsec String u Exp
qop = do
    q <- optionMaybe qualifier
    op <- operator hs
    return $ VarE (mkName $ maybe "" (\q' -> q' ++ ".") q ++ op)

qvarop :: Parsec String u Exp
qvarop = do
    (symbol hs) "`"
    q <- optionMaybe qualifier
    op <- identifier hs
    (symbol hs) "`"
    return $ VarE (mkName $ maybe "" (\q' -> q' ++ ".") q ++ op)

ope :: Parsec String u Exp
ope = try qop <|> qvarop

qvar :: Parsec String u Exp
qvar = do
    q <- optionMaybe qualifier
    op <- identifier hs
    return $ VarE (mkName $ maybe "" (\q' -> q' ++ ".") q ++ op)

tupleexp :: Parsec String u Exp
tupleexp = do
    vs <- (commaSep hs) infixexp
    return $ TupE vs

listexp :: Parsec String u Exp
listexp = do
    vs <- (commaSep hs) infixexp
    return $ ListE vs

lsec :: Parsec String u Exp
lsec = do
    let plsec = sequence [infixexp, ope]
    vop <- (parens hs) plsec
    return $ InfixE (Just $ vop !! 0) (vop !! 1) Nothing

rsec :: Parsec String u Exp
rsec = do
    let prsec = sequence [(parens hs) ope, infixexp]
    opv <- (parens hs) prsec
    return $ InfixE Nothing (opv !! 0) (Just $ opv !! 1)

litexp :: Parsec String u Exp
litexp = try (stringLiteral hs >>= return . LitE . StringL)
     <|> try (charLiteral hs >>= return . LitE . CharL)
     <|> try (integer hs >>= return . LitE . IntegerL)
     <|> (float hs >>= return . LitE . DoublePrimL . toRational)

aexp :: Parsec String u Exp
aexp = try qvar
   <|> try litexp
   <|> try ((parens hs) infixexp)
   <|> try ((parens hs) tupleexp)
   <|> try ((brackets hs) listexp)
   <|> try lsec
   <|> rsec

fexp :: Parsec String u Exp
fexp = do
    as <- many1 aexp
    return $ app as
    where
        app :: [Exp] -> Exp
        app (a:[]) = a
        app (f:a:fs) = app ((AppE f a) : fs)

apat :: Parsec String u Pat
apat = try asp
   <|> try varp
   <|> try litp
   <|> try wcp
   <|> try ((parens hs) pat)
   <|> try ((parens hs) tuplep)
   <|> (brackets hs) listp

pat :: Parsec String u Pat
pat = try consp
  <|> lpat

lpat :: Parsec String u Pat
lpat = apat

asp :: Parsec String u Pat
asp = do
    v <- identifier hs <* string "@"
    p <- apat
    return $ AsP (mkName v) p

varp :: Parsec String u Pat
varp = identifier hs >>= return . VarP . mkName

litp :: Parsec String u Pat
litp = try (stringLiteral hs >>= return . LitP . StringL)
   <|> try (charLiteral hs >>= return . LitP . CharL)
   <|> try (integer hs >>= return . LitP . IntegerL)
   <|> (float hs >>= return . LitP . DoublePrimL . toRational)

wcp :: Parsec String u Pat
wcp = (symbol hs) "_" *> return WildP

tuplep :: Parsec String u Pat
tuplep = do
    vs <- (commaSep hs) pat
    return $ TupP vs

listp :: Parsec String u Pat
listp = do
    vs <- (commaSep hs) pat
    return $ ListP vs

consp :: Parsec String u Pat
consp = do
    l <- lpat
    (symbol hs) ":"
    r <- pat
    return $ InfixP l '(:) r

lambda :: Parsec String u Exp
lambda = do
    (symbol hs) "\\"
    vs <- (commaSep hs) apat
    (symbol hs) "->"
    e <- infixexp
    return $ LamE vs e

plet :: Parsec String u Exp
plet = do
    (symbol hs) "let"
    ds <- (commaSep hs) decl
    (symbol hs) "in"
    e <- infixexp
    return $ LetE ds e

decl :: Parsec String u Dec
decl = do
    p <- pat
    (symbol hs) "="
    e <- infixexp
    return $ ValD p (NormalB e) []

pif :: Parsec String u Exp
pif = do
    (symbol hs) "if"
    pred <- infixexp
    optional (semi hs)
    (symbol hs) "then"
    et <- infixexp
    optional (semi hs)
    (symbol hs) "else"
    ef <- infixexp
    return $ CondE pred et ef

pcase :: Parsec String u Exp
pcase = do
    (symbol hs) "case"
    e <- infixexp
    (symbol hs) "of"
    ms <- (commaSep hs) alt
    return $ CaseE e ms

alt :: Parsec String u Match
alt = do
    p <- pat
    (symbol hs) "->"
    e <- infixexp
    return $ Match p (NormalB e) []

lexp :: Parsec String u Exp
lexp = try lambda
   <|> try plet
   <|> try pif
   <|> try pcase
   <|> fexp

infixexp :: Parsec String u Exp
infixexp = try (InfixE <$> (lexp >>= return . Just) <*> ope <*> (infixexp >>= return . Just))
       <|> try minus
       <|> lexp

getExp :: (E.MonadThrow m)
       => String
       -> m Exp
getExp s = do
    case parse infixexp "" s of
        Left e -> E.throw $ TemplateExprException (show e)
        Right r -> return r

tyvar :: Parsec String u Type
tyvar = do
    try $ lookAhead lower
    v <- identifier hs
    return $ VarT (mkName v)

gtycon :: Parsec String u Type
gtycon = do
    q <- optionMaybe $ try qualifier
    try $ lookAhead upper
    con <- identifier hs
    return $ ConT (mkName $ maybe "" (\q' -> q' ++ ".") q ++ con)

tupt :: Parsec String u Type
tupt = do
    ts <- ((parens hs) ((commaSep1 hs) typ))
    return $ app ((TupleT $ length ts) : ts)
    where
        app :: [Type] -> Type
        app (a:[]) = a
        app (a:b:ts) = app ((AppT a b) : ts)

listt :: Parsec String u Type
listt = do
    t <- (brackets hs) typ
    return $ AppT ListT t 

btype :: Parsec String u Type
btype = do
    as <- many1 atype
    return $ app as
    where
        app :: [Type] -> Type
        app (a:[]) = a
        app (a:b:ts) = app ((AppT a b) : ts)

atype :: Parsec String u Type
atype = try gtycon
    <|> try tyvar
    <|> try tupt
    <|> try listt
    <|> ((parens hs) typ)

typ :: Parsec String u Type
typ = btype

getType :: String
        -> Maybe Type
getType s = case parse typ "" s of
                Left e -> Nothing
                Right r -> Just r
