module Route exposing (Route(..), fromUrl, toUrl)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Route
    = New
    | Edit String
    | Play String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Edit (s "edit" </> string)
        , map Play (s "play" </> string)
        , map New top
        ]


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url


toUrl : Route -> String
toUrl route =
    case route of
        New ->
            "/"

        Edit key ->
            "/edit/" ++ key

        Play key ->
            "/play/" ++ key
