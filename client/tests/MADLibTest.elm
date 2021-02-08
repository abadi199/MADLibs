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

        testHelper expected text =
            test text <|
                \_ ->
                    Parser.run MADLib.inputTokenParser text
                        |> Expect.equal (Result.Ok <| expected)
    in
    describe "inputTokenParser"
        [ ("[" ++ label ++ "]")
            |> testHelper (MADLib.Input label)
        , ("[" ++ label ++ "] last")
            |> testHelper (MADLib.Input label)
        , ("[" ++ label)
            |> testHelper (MADLib.Text <| "[" ++ label)
        ]


textTokenParserSuite : Test
textTokenParserSuite =
    describe "textTokenParser"
        [ test "empty string" <|
            \_ ->
                Parser.run MADLib.textTokenParser ""
                    |> Expect.equal (Result.Ok <| MADLib.Text "")
        , let
            text =
                "] this is a test"
          in
          test text <|
            \_ ->
                Parser.run MADLib.textTokenParser ("]" ++ text)
                    |> Expect.equal (Result.Ok <| MADLib.Text <| "]" ++ text)
        , let
            text =
                "this is a test"
          in
          test text <|
            \_ ->
                Parser.run MADLib.textTokenParser text
                    |> Expect.equal (Result.Ok <| MADLib.Text text)
        , test "something something [ label " <|
            \_ ->
                Parser.run MADLib.textTokenParser "something something [ label"
                    |> Expect.equal (Result.Ok <| MADLib.Text "something something ")
        , test "start with opening bracket" <|
            \_ ->
                Parser.run MADLib.textTokenParser "[label"
                    |> Expect.equal (Result.Ok <| MADLib.Text "")
        ]


parseSuite : Test
parseSuite =
    describe "parse"
        [ test "empty text should empty list" <|
            \_ ->
                MADLib.parse "" |> Expect.equal (Result.Ok [])
        , let
            text =
                "this is a test"
          in
          test text <|
            \_ ->
                MADLib.parse text |> Expect.equal (Result.Ok [ MADLib.Text text ])
        , let
            text =
                "this is a test ]"
          in
          test text <|
            \_ ->
                MADLib.parse text |> Expect.equal (Result.Ok [ MADLib.Text "this is a test" ])
        , let
            text =
                "[this is a test"
          in
          test text <|
            \_ ->
                MADLib.parse text |> Expect.equal (Result.Ok [ MADLib.Text "this is a test" ])
        , let
            text =
                "[]"
          in
          test text <|
            \_ ->
                MADLib.parse text |> Expect.equal (Result.Ok [ MADLib.Input "" ])
        , let
            label =
                "this is a test"

            text =
                "[" ++ label ++ "]"
          in
          test text <|
            \_ ->
                MADLib.parse text |> Expect.equal (Result.Ok [ MADLib.Input label ])
        , let
            text =
                "some sentence [] more sentence"
          in
          test text <|
            \_ ->
                MADLib.parse text
                    |> Expect.equal
                        (Result.Ok
                            [ MADLib.Text "some sentence"
                            , MADLib.Input ""
                            , MADLib.Text "more sentence"
                            ]
                        )
        , let
            text =
                "some sentence [ this is a label] more sentence"
          in
          test text <|
            \_ ->
                MADLib.parse text
                    |> Expect.equal
                        (Result.Ok
                            [ MADLib.Text "some sentence"
                            , MADLib.Input "this is a label"
                            , MADLib.Text "more sentence"
                            ]
                        )
        , let
            text =
                """I am working from home these days. I have been [adjective] and a little [adjective]. My routine looks
different than it did when I was going into the office every day. Now the first thing I do when I wake up is [verb] [noun] and put on [clothing item]. Then I have [type of food] for breakfast and get to work. I have a lot of virtual meetings during the day. The most [adjective] thing to happen to me during one of them was when my [family member] walked in during a client meeting with [client] wearing a [noun] on their [body part].

In the afternoons, I try to send at least [number] GIF’s to [1904labs person], to let them know I’m thinking of them and make them feel [emotion]. I think it’s [adjective] that they only respond to me with [type of animal] GIF’s. When I need a break from my computer, I go to the [room in your house] and [verb] [noun]. Sometimes I take extended breaks to practice juggling [plural noun] or binge-watch [TV show] while eating a big tub of [type of food]. Overall, I really [verb to express emotion] working from home. I hope that we go back to the office in [month]."""
          in
          test "story" <|
            \_ ->
                MADLib.parse text
                    |> Expect.equal
                        (Result.Ok
                            [ MADLib.Text "I am working from home these days. I have been"
                            , MADLib.Input "adjective"
                            , MADLib.Text "and a little"
                            , MADLib.Input "adjective"
                            , MADLib.Text ". My routine looks\ndifferent than it did when I was going into the office every day. Now the first thing I do when I wake up is"
                            , MADLib.Input "verb"
                            , MADLib.Input "noun"
                            , MADLib.Text "and put on"
                            , MADLib.Input "clothing item"
                            , MADLib.Text ". Then I have"
                            , MADLib.Input "type of food"
                            , MADLib.Text "for breakfast and get to work. I have a lot of virtual meetings during the day. The most"
                            , MADLib.Input "adjective"
                            , MADLib.Text "thing to happen to me during one of them was when my"
                            , MADLib.Input "family member"
                            , MADLib.Text "walked in during a client meeting with"
                            , MADLib.Input "client"
                            , MADLib.Text "wearing a"
                            , MADLib.Input "noun"
                            , MADLib.Text "on their"
                            , MADLib.Input "body part"
                            , MADLib.Text ".\n\nIn the afternoons, I try to send at least"
                            , MADLib.Input "number"
                            , MADLib.Text "GIF’s to"
                            , MADLib.Input "1904labs person"
                            , MADLib.Text ", to let them know I’m thinking of them and make them feel"
                            , MADLib.Input "emotion"
                            , MADLib.Text ". I think it’s"
                            , MADLib.Input "adjective"
                            , MADLib.Text "that they only respond to me with"
                            , MADLib.Input "type of animal"
                            , MADLib.Text "GIF’s. When I need a break from my computer, I go to the"
                            , MADLib.Input "room in your house"
                            , MADLib.Text "and"
                            , MADLib.Input "verb"
                            , MADLib.Input "noun"
                            , MADLib.Text ". Sometimes I take extended breaks to practice juggling"
                            , MADLib.Input "plural noun"
                            , MADLib.Text "or binge-watch"
                            , MADLib.Input "TV show"
                            , MADLib.Text "while eating a big tub of"
                            , MADLib.Input "type of food"
                            , MADLib.Text ". Overall, I really"
                            , MADLib.Input "verb to express emotion"
                            , MADLib.Text "working from home. I hope that we go back to the office in"
                            , MADLib.Input "month"
                            , MADLib.Text "."
                            ]
                        )
        ]


removeString : String -> Fuzzer String -> Fuzzer String
removeString str fuzzer =
    fuzzer |> Fuzz.map (String.replace str "")
