module MADLib exposing (MADLib, Token(..), inputTokenParser, parse, textTokenParser, viewToken)

import Element as E
import Element.Font as Font
import Html as H
import Parser exposing ((|.), (|=), getChompedString)


type alias MADLib =
    List Token


type Token
    = Text String
    | Input String
    | AnsweredInput { label : String, answer : String }


parse : String -> Result (H.Html msg) MADLib
parse text =
    text
        |> Parser.run parser
        |> Result.mapError toHtml


parser : Parser.Parser MADLib
parser =
    Parser.loop [] parserHelper


clean : String -> String
clean text =
    [ "]", "[" ]
        |> List.foldl (\reject acc -> String.replace reject "" acc) text
        |> String.trim


parserHelper : MADLib -> Parser.Parser (Parser.Step MADLib MADLib)
parserHelper madlib =
    Parser.oneOf
        [ Parser.succeed
            (\token ->
                case token of
                    Text "" ->
                        madlib |> List.reverse |> Parser.Done

                    Text text ->
                        let
                            cleanText =
                                clean text
                        in
                        if cleanText == "" then
                            Parser.Loop madlib

                        else
                            Parser.Loop (Text cleanText :: madlib)

                    Input label ->
                        Parser.Loop (Input (clean label) :: madlib)

                    _ ->
                        Parser.Loop (token :: madlib)
            )
            |= tokenParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse madlib))
        ]


tokenParser : Parser.Parser Token
tokenParser =
    Parser.oneOf
        [ inputTokenParser
        , textTokenParser
        ]


inputTokenParser : Parser.Parser Token
inputTokenParser =
    Parser.succeed identity
        |. Parser.symbol "["
        |= Parser.oneOf
            [ closingBracketFound
            , noClosingBracket
            ]


closingBracketFound : Parser.Parser Token
closingBracketFound =
    Parser.chompUntil "]" |> Parser.getChompedString |> Parser.map Input


noClosingBracket : Parser.Parser Token
noClosingBracket =
    Parser.succeed (\text -> "[" ++ text |> Text)
        |= (Parser.chompUntilEndOr "\n" |> Parser.getChompedString)
        |. Parser.end


textTokenParser : Parser.Parser Token
textTokenParser =
    Parser.succeed ()
        |. Parser.chompUntilEndOr "["
        |> getChompedString
        |> Parser.map Text


toHtml : List Parser.DeadEnd -> H.Html msg
toHtml deadEnds =
    H.text "DEAD ENDS"


viewToken : Token -> E.Element msg
viewToken token =
    case token of
        Text text ->
            E.text text

        Input label ->
            E.el [ Font.bold, E.padding 10 ] (E.text label)

        AnsweredInput { answer } ->
            E.el [ Font.bold ] (E.text answer)
