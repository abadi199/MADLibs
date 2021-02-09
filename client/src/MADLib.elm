module MADLib exposing
    ( MADLib
    , Token(..)
    , filterByInput
    , inputTokenParser
    , isAnswered
    , parse
    , textTokenParser
    , viewToken
    , viewTokenForm
    )

import Element as E
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
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
toHtml _ =
    H.text "DEAD ENDS"


viewToken : Token -> E.Element msg
viewToken token =
    case token of
        Text text ->
            E.el [ E.paddingXY 5 0 ] (E.text text)

        Input label ->
            E.el
                [ Font.bold
                , E.paddingXY 5 0
                , Background.color (E.rgba255 222 244 64 0.25)
                ]
                (E.text <| "[" ++ label ++ "]")

        AnsweredInput { answer } ->
            E.el
                [ Font.bold
                , E.paddingXY 5 0
                , Background.color (E.rgba255 0 254 45 0.25)
                ]
                (E.text answer)


viewTokenForm : (Int -> Token -> msg) -> Int -> Token -> Maybe (E.Element msg)
viewTokenForm onChange index token =
    let
        viewAnswer label answer =
            E.el []
                (Input.text [ E.width (E.px 300) ]
                    { onChange = \newAnswer -> onChange index (AnsweredInput { label = label, answer = newAnswer })
                    , text = answer
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (E.text label)
                    }
                )
    in
    case token of
        Input label ->
            Just <| viewAnswer label ""

        AnsweredInput { label, answer } ->
            Just <| viewAnswer label answer

        _ ->
            Nothing


isInput : Token -> Bool
isInput token =
    case token of
        Input _ ->
            True

        _ ->
            False


filterByInput : MADLib -> MADLib
filterByInput madlib =
    madlib |> List.filter (\token -> isInput token)


isAnswered : Token -> Bool
isAnswered token =
    case token of
        AnsweredInput { answer } ->
            String.isEmpty answer |> not

        Input _ ->
            False

        _ ->
            True
