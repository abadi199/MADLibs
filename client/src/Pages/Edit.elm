module Pages.Edit exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , updateSession
    , view
    )

import Api
import Browser.Navigation
import Button
import Element as E
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html as H
import MADLib exposing (MADLib)
import RemoteData exposing (WebData)
import Route
import Session exposing (Session)


defaultStory : String
defaultStory =
    "Hi [Name], start typing your story here. Use square bracket to mark an input word. Have fun, [Noun]!"


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


updateSession : Session -> Model -> Model
updateSession session model =
    { model | session = session }


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
            , E.paddingXY 0 40
            , E.spacing 40
            , E.width (E.fill |> E.maximum 1000)
            , E.centerX
            ]
            [ E.el [ Region.heading 1, Font.size 30, Font.bold ] (E.text "MADLabs")
            , E.row [ E.spacing 20, E.width E.fill ]
                [ Input.multiline [ E.height E.fill, E.width <| E.px 500 ]
                    { onChange = TextUpdated
                    , text = RemoteData.withDefault "" model.text
                    , label = Input.labelAbove [] (E.text "Text")
                    , placeholder = Nothing
                    , spellcheck = True
                    }
                , preview model.madlib
                ]
            , E.el [ E.width E.fill ]
                (E.row [ E.spacing 10, E.centerX ]
                    [ Button.button
                        (case model.save of
                            RemoteData.Loading ->
                                "Saving..."

                            _ ->
                                "Save"
                        )
                        SaveButtonClicked
                    , Button.button
                        "Play"
                        PlayButtonClicked
                    ]
                )
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
            let
                updatedText =
                    text
                        |> RemoteData.map
                            (\story ->
                                if String.isEmpty story then
                                    defaultStory

                                else
                                    story
                            )
                        |> RemoteData.withDefault defaultStory
                        |> RemoteData.Success
            in
            ( { model
                | text = updatedText
                , madlib =
                    updatedText
                        |> RemoteData.toMaybe
                        |> Maybe.map MADLib.parse
                        |> Maybe.withDefault (Result.Ok [])
              }
            , Cmd.none
            )

        OnSaveComplete save ->
            ( { model | save = save }, Cmd.none )

        PlayButtonClicked ->
            ( model.text
                |> RemoteData.toMaybe
                |> Maybe.map (\text -> { model | session = Session.cacheStory text model.session })
                |> Maybe.withDefault model
            , Cmd.batch
                [ model.text |> RemoteData.map (\text -> Api.save { key = model.key, text = text } OnSaveComplete) |> RemoteData.withDefault Cmd.none
                , Browser.Navigation.pushUrl navKey (Route.toUrl (Route.Play model.key))
                ]
            )
