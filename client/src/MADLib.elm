module MADLib exposing (MADLib, Token(..), inputTokenParser, parse, textTokenParser)

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


parserHelper : MADLib -> Parser.Parser (Parser.Step MADLib MADLib)
parserHelper madlib =
    Parser.oneOf
        [ Parser.succeed (\token -> Parser.Loop (token :: madlib))
            |= tokenParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse madlib))
        ]


tokenParser : Parser.Parser Token
tokenParser =
    Parser.oneOf
        [ textTokenParser
        , inputTokenParser
        ]


inputTokenParser : Parser.Parser Token
inputTokenParser =
    Parser.succeed ()
        |. Parser.symbol "["
        |. Parser.chompUntil "]"
        |> getChompedString
        |> Parser.map (String.dropLeft 1 >> Input)


textTokenParser : Parser.Parser Token
textTokenParser =
    Parser.succeed ()
        |. Parser.chompUntilEndOr "["
        |> getChompedString
        |> Parser.map Text


toHtml : List Parser.DeadEnd -> H.Html msg
toHtml deadEnds =
    let
        _ =
            Debug.log "parser error" deadEnds
    in
    H.text (Debug.toString deadEnds)
