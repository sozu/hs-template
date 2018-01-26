module Data.Template.ModelSpec where

import Test.Hspec
import Data.Template.Model

spec :: Spec
spec = do
    describe "Interpret a template as a tree" $ do
        it "HTML style" $ makeHierarchy testHtmlTemplate `shouldBe` testTree

testHtmlTemplate :: [TemplateToken]
testHtmlTemplate = [ Raw "a"
      , TagBegin (Block "A")
      , Raw "b"
      , TagEnd
      , Raw "c"
      , TagAutonomous (Import "B")
      , Raw "d"
      , TagAutonomous (Def "C" "D")
      , Raw "e"
      , TagAutonomous (Call "E" ["F"])
      , Raw "f"
      , TagAutonomous (Block "G")
      , Raw "g"
      , TagBegin (For "H" "I" (Just "J"))
      , Raw "h"
      , TagBegin (Eval "K")
      , Raw "i"
      , TagBegin (Eval "L")
      , Raw "j"
      , TagBegin (If "M")
      , TagAutonomous (Def "N" "O")
      , Raw "k"
      , TagBegin (ElseIf "P")
      , Raw "l"
      , TagBegin Else
      , Raw "m"
      , TagEnd
      , Raw "n"
      , TagEnd
      , Raw "o"
      , TagAutonomous (Includes "Q")
      , Raw "p"
      ]

testTree :: [TagNode]
testTree = [ RawNode "a"
      , TagNode (Block "A") [
          RawNode "b"
        ]
      , RawNode "c"
      , TagNode (Import "B") []
      , RawNode "d"
      , TagNode (Def "C" "D") [
          RawNode "e"
        , TagNode (Call "E" ["F"]) []
        , RawNode "f"
        , TagNode (Block "G") []
        , RawNode "g"
        , TagNode (For "H" "I" (Just "J")) [
            RawNode "h"
          , TagNode (Eval "K") []
          , RawNode "i"
          , TagNode (Eval "L") []
          , RawNode "j"
          , IfNode [
                ("M", [TagNode (Def "N" "O") [RawNode "k"]])
              , ("P", [RawNode "l"])
              ] (Just [RawNode "m"])
          , RawNode "n"
          ]
        , RawNode "o"
        , TagNode (Includes "Q") []
        , RawNode "p"
        ]
      ]