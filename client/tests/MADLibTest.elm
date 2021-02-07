module MADLibTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer, string)
import MADLib
import Parser
import Test exposing (..)


inputTokenParserSuite : Test
inputTokenParserSuite =
    let
        label =
            "test"

        testHelper text =
            test text <|
                \_ ->
                    Parser.run MADLib.inputTokenParser text
                        |> Expect.equal (Result.Ok <| MADLib.Input label)
    in
    describe "inputTokenParser"
        [ ("[" ++ label ++ "]") |> testHelper
        , ("[" ++ label ++ "] last") |> testHelper
        ]


textTokenParserSuite : Test
textTokenParserSuite =
    Test.only <|
        describe "textTokenParser"
            [ test "empty string" <|
                \_ ->
                    Parser.run MADLib.textTokenParser ""
                        |> Expect.equal (Result.Ok <| MADLib.Text "")
            , fuzz stringWithoutBracket "text with no square bracket" <|
                \text ->
                    Parser.run MADLib.textTokenParser text
                        |> Expect.equal (Result.Ok <| MADLib.Text text)
            ]


parseSuite : Test
parseSuite =
    describe "parse"
        [ Test.only <|
            test "empty text should return single empty Text" <|
                \_ ->
                    MADLib.parse ""
                        |> Expect.equal (Result.Ok [ MADLib.Text "" ])
        , fuzz stringWithoutBracket "text with no square bracket should return single Text" <|
            \text ->
                MADLib.parse text
                    |> Expect.equal (Result.Ok [ MADLib.Text text ])
        , fuzz stringWithoutOpeningBracket "text with no opening bracket should return single Text" <|
            \text ->
                MADLib.parse ("]" ++ text)
                    |> Expect.equal (Result.Ok [ MADLib.Text ("]" ++ text) ])
        , fuzz stringWithoutClosingBracket "text with no closing bracket should return single Text" <|
            \text ->
                MADLib.parse ("[" ++ text)
                    |> Expect.equal (Result.Ok [ MADLib.Text ("[" ++ text) ])
        , test "text with empty bracket should return single Input" <|
            \_ ->
                MADLib.parse "[]"
                    |> Expect.equal (Result.Ok [ MADLib.Input "" ])
        , fuzz stringWithoutBracket "text with single bracket should return single Input" <|
            \label ->
                MADLib.parse ("[" ++ label ++ "]")
                    |> Expect.equal (Result.Ok [ MADLib.Input label ])
        ]


stringWithoutOpeningBracket : Fuzzer String
stringWithoutOpeningBracket =
    string
        |> removeString "["


stringWithoutClosingBracket : Fuzzer String
stringWithoutClosingBracket =
    string
        |> removeString "]"


stringWithoutBracket : Fuzzer String
stringWithoutBracket =
    string
        |> removeString "["
        |> removeString "]"


removeString : String -> Fuzzer String -> Fuzzer String
removeString str fuzzer =
    fuzzer |> Fuzz.map (String.replace str "")
