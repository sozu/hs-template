module Data.Template.ParserSpec where

import qualified Data.Map as M
import Test.Hspec
import Text.Parsec
import Data.Template.Parser
import Data.Template.Model

spec :: Spec
spec = do
    describe "Parser of individual tag" $ do
        it "Eval"       $ testTag "{- length str * 4 + 2 -}" (Eval "length str * 4 + 2 ") False
        it "Def"        $ testTag "{-| var = 4 * 2 -}" (Def "var" "4 * 2 ") False
        it "If"         $ testTag "{-? x < 3 -}" (If "x < 3 ") False
        it "ElseIf"     $ testTag "{-!? x < 10 -}" (ElseIf "x < 10 ") False
        it "Else"       $ testTag "{-!-}" Else False
        it "For"        $ testTag "{-% i v [1, 2, 3] -}" (For "v" "[1, 2, 3] " (Just "i")) False
        it "Import"     $ testTag "{-& macros.html -}" (Import "macros.html") False
        it "Includes"   $ testTag "{-< components.html -}" (Includes "components.html") False
        it "Block"      $ testTag "{-@ contents -}" (Block "contents") False
        it "Macro"      $ testTag "{-* my_macro a1 a_2 a3 -}" (Macro "my_macro" ["a1", "a_2", "a3"]) False
        it "Call"       $ testTag "{-= my_macro v_1 2 v3 -}" (Call "my_macro" ["v_1", "2", "v3"]) False
        it "End"        $ testTag "{-$-}" End False

    describe "Autonomous tag" $ do
        it "Eval"       $ testTag "{- length str * 4 + 2 $-}" (Eval "length str * 4 + 2 ") True
        it "Def"        $ testTag "{-| var = 4 * 2 $-}" (Def "var" "4 * 2 ") True
        it "Import"     $ testTag "{-& macros.html $-}" (Import "macros.html") True
        it "Includes"   $ testTag "{-< components.html $-}" (Includes "components.html") True
        it "Block"      $ testTag "{-@ contents $-}" (Block "contents") True
        it "Call"       $ testTag "{-= my_macro v_1 2 v3 $-}" (Call "my_macro" ["v_1", "2", "v3"]) True

    describe "Directive" $ do
        it "Extends"    $ testDirective "{-# extends base.html #-}" (Extends "base.html")
        it "Encoding"   $ testDirective "{-# encoding UTF-8 #-}" (Encoding "UTF-8")

    describe "Remove spaces surrounding a tag when a line includes them only" $ do
        it "Remove surrounding spaces" $ do
            removeTagLine [ Raw "abc\r\n    "
                          , TagBegin (Block "1")
                          , Raw "    \r\ndef"
                          ] `shouldBe` [Raw "abc\r\n", TagBegin (Block "1"), Raw "def"]

        it "Any character which is not a space prevents the removal" $ do
            removeTagLine [ Raw "abc\r\n  x  "
                          , TagBegin (Block "1")
                          , Raw "    \r\ndef"
                          ] `shouldBe` [Raw "abc\r\n  x  ", TagBegin (Block "1"), Raw "    \r\ndef"]
            removeTagLine [ Raw "abc\r\n    "
                          , TagBegin (Block "1")
                          , Raw "  x  \r\ndef"
                          ] `shouldBe` [Raw "abc\r\n    ", TagBegin (Block "1"), Raw "  x  \r\ndef"]

        it "If a string of the next token becomes empty as a result of removal, the token itself is removed" $ do
            removeTagLine [ Raw "\r\n    "
                          , TagBegin (Block "1")
                          , Raw "    \r\n"
                          ] `shouldBe` [Raw "\r\n", TagBegin (Block "1")]

        it "Spaces are removed from both side of a token" $ do
            removeTagLine [ TagBegin (Block "1")
                          , Raw "    \r\nabc\r\n   "
                          , TagBegin (Block "2")
                          , Raw "    \r\n"
                          ] `shouldBe` [TagBegin (Block "1"), Raw "abc\r\n", TagBegin (Block "2")]

            removeTagLine [ TagBegin (Block "1")
                          , Raw "    \r\n   "
                          , TagBegin (Block "2")
                          , Raw "    \r\n"
                          ] `shouldBe` [TagBegin (Block "1"), TagBegin (Block "2")]

    describe "Template" $ do
        it "HTML style" $ parseTemplate testHtml `shouldBe` Right testHtmlTemplate

    describe "Resolve" $ do
        it "Hierarchical text template" $ do
            let template = parseTemplateFile "data/test/import1.txt"
            let c = newContext { rootPath = "data/test" }
            c' <- resolveContext c True template 
            M.toList (blocks c') `shouldContain` [("b0", [RawNode "abc\r\n"]), ("b1", [RawNode "---\r\n"])]
            M.toList (macros c') `shouldContain` [ ("macro1", (["s"], [TagNode (Eval "s ") [], RawNode "\r\n"]))
                                                 , ("macro2", (["a1", "a2"], [ RawNode "macro2 "
                                                                             , TagNode (Eval "a1 ") []
                                                                             , RawNode " "
                                                                             , TagNode (Eval "a2 ") []
                                                                             , RawNode "\r\n"
                                                                             ]))
                                                 ]

testTag :: String
        -> Tag
        -> Bool
        -> IO ()
testTag s t b = parse parseTag "" s `shouldBe` Right (t, b)

testDirective :: String
              -> Directive
              -> IO ()
testDirective s d = parse parseDirective "" (s ++ "\r\n") `shouldBe` Right d

testHtml :: String
testHtml = "\
\{-# extends base.html #-}\n\
\{-# encoding UTF-8 #-}\n\
\<html>\n\
\    <head>\n\
\        {-@ head -}\n\
\        <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n\
\        {-$-}\n\
\    </head>\n\
\    <body>\n\
\        {-& macros.html $-}\n\
\        {-| account = user session $-}\n\
\        {-= header account $-}\n\
\        <h1>{-@ title $-}</h1>\n\
\        <table>\n\
\            <tr>\n\
\                <th>ID</th>\n\
\                <th>Name</th>\n\
\                <th>Age</th>\n\
\            </tr>\n\
\            {-% i v values -}\n\
\            <tr>\n\
\                <td>{- i -}</td>\n\
\                <td>{- name v -}</td>\n\
\                <td>\n\
\                    {-? age v < 20 -}under 20\n\
\                    {-!? age v < 50 -}20 - 49\n\
\                    {-!-}over 50{-$-}\n\
\                </td>\n\
\            </tr>\n\
\            {-$-}\n\
\        </table>\n\
\        {-< footer.html $-}\n\
\    </body>\n\
\</html>\n\
\"

testHtmlTemplate :: Template
testHtmlTemplate = (
    [Extends "base.html", Encoding "UTF-8"]
    , [ Raw "<html>\n\
            \    <head>\n\
            \"
      , TagBegin (Block "head")
      , Raw "        <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n\
            \"
      , TagEnd
      , Raw "    </head>\n\
            \    <body>\n\
            \"
      , TagAutonomous (Import "macros.html")
      , TagAutonomous (Def "account" "user session ")
      , TagAutonomous (Call "header" ["account"])
      , Raw "        <h1>\
            \"
      , TagAutonomous (Block "title")
      , Raw "</h1>\n\
            \        <table>\n\
            \            <tr>\n\
            \                <th>ID</th>\n\
            \                <th>Name</th>\n\
            \                <th>Age</th>\n\
            \            </tr>\n\
            \"
      , TagBegin (For "v" "values " (Just "i"))
      , Raw "            <tr>\n\
            \                <td>\
            \"
      , TagBegin (Eval "i ")
      , Raw "</td>\n\
            \                <td>\
            \"
      , TagBegin (Eval "name v ")
      , Raw "</td>\n\
            \                <td>\n\
            \                    \
            \"
      , TagBegin (If "age v < 20 ")
      , Raw "under 20\n\
            \                    \
            \"
      , TagBegin (ElseIf "age v < 50 ")
      , Raw "20 - 49\n\
            \                    \
            \"
      , TagBegin Else
      , Raw "over 50"
      , TagEnd
      , Raw "\n\
            \                </td>\n\
            \            </tr>\n\
            \"
      , TagEnd
      , Raw "        </table>\n\
            \        \
            \"
      , TagAutonomous (Includes "footer.html")
      , Raw "\n    </body>\n\
            \</html>\n\
            \"
      ]
    )