module Pages.Edit exposing (Model, Msg, init, toSession, update, view)

import Api
import Browser.Navigation
import Element as E
import Element.Input as Input
import Html as H
import MADLib exposing (MADLib)
import RemoteData exposing (WebData)
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , key : String
    , madlib : Result (H.Html Msg) MADLib
    , text : WebData String
    , save : WebData ()
    }


type Msg
    = TextUpdated String
    | SaveButtonClicked
    | PlayButtonClicked
    | OnLoadComplete (WebData String)
    | OnSaveComplete (WebData ())


toSession : Model -> Session
toSession { session } =
    session


init : Session -> String -> ( Model, Cmd Msg )
init session key =
    ( { session = session
      , key = key
      , madlib = Result.Ok []
      , text = RemoteData.NotAsked
      , save = RemoteData.NotAsked
      }
    , Api.load key OnLoadComplete
    )


view : Model -> List (H.Html Msg)
view model =
    [ E.layout []
        (E.column
            [ E.width E.fill
            , E.height E.fill
            , E.padding 20
            , E.spacing 40
            , E.width (E.fill |> E.maximum 1000)
            , E.centerX
            ]
            [ Input.multiline [ E.height E.fill, E.width E.fill ]
                { onChange = TextUpdated
                , text = RemoteData.withDefault "" model.text
                , label = Input.labelAbove [] (E.text "Text")
                , placeholder = Nothing
                , spellcheck = True
                }
            , preview model.madlib
            , E.row [ E.spacing 10 ]
                [ Input.button []
                    { onPress = Just SaveButtonClicked
                    , label =
                        case model.save of
                            RemoteData.Loading ->
                                E.text "Saving..."

                            _ ->
                                E.text "Save"
                    }
                , Input.button []
                    { onPress = Just PlayButtonClicked
                    , label = E.text "Play"
                    }
                ]
            ]
        )
    ]


preview : Result (H.Html Msg) MADLib -> E.Element Msg
preview madlibResult =
    case madlibResult of
        Ok madlib ->
            madlib
                |> List.map MADLib.viewToken
                |> E.paragraph []

        Err error ->
            E.html error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        navKey =
            model.session |> Session.navKey
    in
    case msg of
        TextUpdated text ->
            ( { model
                | text = RemoteData.Success text
                , madlib = MADLib.parse text
              }
            , Cmd.none
            )

        SaveButtonClicked ->
            case model.text of
                RemoteData.Success text ->
                    ( { model | save = RemoteData.Loading }, Api.save { key = model.key, text = text } OnSaveComplete )

                _ ->
                    ( model, Cmd.none )

        OnLoadComplete text ->
            ( { model
                | text = text
                , madlib =
                    text
                        |> RemoteData.toMaybe
                        |> Maybe.map MADLib.parse
                        |> Maybe.withDefault (Result.Ok [])
              }
            , Cmd.none
            )

        OnSaveComplete save ->
            ( { model | save = save }, Cmd.none )

        PlayButtonClicked ->
            ( model, Browser.Navigation.pushUrl navKey (Route.toUrl (Route.Play model.key)) )
