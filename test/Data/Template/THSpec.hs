{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE IncoherentInstances #-}

module Data.Template.THSpec where

import Test.Hspec
import Data.Template.Model
import Data.Template.TH

spec :: Spec
spec = do
    describe "Splice expressions of various types of nodes" $ do
        it "Raw node" $ do
            $(generateExp newContext (RawNode "abc")) `shouldBe` "abc"

        it "Eval node with local variable" $ do
            let v = "abc"
            $(
                generateExp newContext (TagNode (Eval "v") [])
             ) `shouldBe` "abc"

        it "Eval node with contextual variable" $ do
            $(
                let c = newContext += ("v", [| "abc" |])
                in generateExp c (TagNode (Eval "v") [])
             ) `shouldBe` "abc"

        it "Eval node including operation" $ do
            let v = 3 :: Int
            $(
                generateExp newContext (TagNode (Eval "v * 5 ") [])
             ) `shouldBe` "15"

        it "Def node" $ do
            let v = "abc"
            $(
                generateExp newContext $
                    TagNode (Def "u" "length v + 2 ")
                            [ RawNode "def"
                            , TagNode (Eval "u * 3 ") []
                            , RawNode "ghi"
                            ]
             ) `shouldBe` "def15ghi"

        it "Block node" $ do
            let v = "abc"
            $(
                let c = newContext +@~ ("block", [ RawNode "def"
                                                 , TagNode (Eval "v") []
                                                 , RawNode "ghi"
                                                 ])
                in generateExp c (TagNode (Block "block") [])
             ) `shouldBe` "defabcghi"

        it "Block node of unknown name" $ do
            let v = "abc"
            $(
                let c = newContext +@~ ("block", [ RawNode "def"
                                                 , TagNode (Eval "v") []
                                                 , RawNode "ghi"
                                                 ])
                in generateExp c (TagNode (Block "block'") [])
             ) `shouldBe` ""

        it "Call macro" $ do
            let v = "abc"
            let n = 5 :: Int
            $(
                let c = newContext +&~ ("macro", ["a1", "a2", "a3"], [ RawNode "def"
                                                                     , TagNode (Eval "length a1") []
                                                                     , TagNode (Eval "a2 * a3") []
                                                                     , RawNode "ghi"
                                                                     ])
                in generateExp c (TagNode (Call "macro" ["v", "3", "n"]) [])
             ) `shouldBe` "def315ghi"

        it "Call macro of unknown name" $ do
            let v = "abc"
            let n = 5
            $(
                let c = newContext +&~ ("macro", ["a1", "a2", "a3"], [ RawNode "def"
                                                                     , TagNode (Eval "length a1") []
                                                                     , TagNode (Eval "a2 * a3") []
                                                                     , RawNode "ghi"
                                                                     ])
                in generateExp c (TagNode (Call "macro'" ["v", "3", "n"]) [])
             ) `shouldBe` ""

        it "For" $ do
            let vs = ["abc", "def", "ghi"]
            $(
                generateExp newContext (TagNode (For "v" "vs" (Just "i")) [ RawNode "A"
                                                                          , TagNode (Eval "i") []
                                                                          , TagNode (Eval "v") []
                                                                          , RawNode "Z"
                                                                          ])
             ) `shouldBe` "A0abcZA1defZA2ghiZ"

        it "If node when first condition is true" $ do
            let (a, b, c) = (True, False, False)
            $(
                generateExp newContext (IfNode [ ("a", [RawNode "A"])
                                               , ("b", [RawNode "B"])
                                               , ("c", [RawNode "C"])
                                               ] (Just [RawNode "D"]))
             ) `shouldBe` "A"

        it "If node when second condition is true" $ do
            let (a, b, c) = (False, True, False)
            $(
                generateExp newContext (IfNode [ ("a", [RawNode "A"])
                                               , ("b", [RawNode "B"])
                                               , ("c", [RawNode "C"])
                                               ] (Just [RawNode "D"]))
             ) `shouldBe` "B"

        it "If node when any condition is not true" $ do
            let (a, b, c) = (False, False, False)
            $(
                generateExp newContext (IfNode [ ("a", [RawNode "A"])
                                               , ("b", [RawNode "B"])
                                               , ("c", [RawNode "C"])
                                               ] (Just [RawNode "D"]))
             ) `shouldBe` "D"

        it "If node without elseif" $ do
            let a = False
            $(
                generateExp newContext (IfNode [ ("a", [RawNode "A"])
                                               ] (Just [RawNode "D"]))
             ) `shouldBe` "D"

        it "If node without else" $ do
            let (a, b) = (False, False)
            $(
                generateExp newContext (IfNode [ ("a", [RawNode "A"])
                                               , ("b", [RawNode "B"])
                                               ] Nothing)
             ) `shouldBe` ""

        it "Include tag" $ do
            let s = "def"
            $(
                let c = newContext { rootPath = "data/test" }
                in generateExp c (TagNode (Includes "include.txt") [])
             ) `shouldBe` "abcdefghijklmno"

    describe "Renders hierarchical text template" $ do
        it "3 level hierarchy, macro import and file inclusion" $ do
            $(
                execTemplateHierarchy generateExp newContext "data/test/import0.txt"
             ) `shouldBe` "abc\r\n\
                          \def\r\n\
                          \ghi\r\n\
                          \jkl\r\n\
                          \mno\r\n\
                          \pqr\r\n\
                          \stu\r\n\
                          \vwx\r\n\
                          \"

