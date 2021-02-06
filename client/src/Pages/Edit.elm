module Pages.Edit exposing (Model, Msg, init, toSession, update, view)

import Api
import Element as E
import Element.Input as Input
import Html
import RemoteData exposing (WebData)
import Session exposing (Session)


type alias Model =
    { session : Session
    , key : String
    , text : WebData String
    }


type Msg
    = TextUpdated String
    | SaveButtonClicked
    | OnLoadComplete (WebData String)
    | OnSaveComplete (WebData ())


toSession : Model -> Session
toSession { session } =
    session


init : Session -> String -> ( Model, Cmd Msg )
init session key =
    ( { session = session
      , key = key
      , text = RemoteData.NotAsked
      }
    , Api.load key OnLoadComplete
    )


view : Model -> List (Html.Html Msg)
view model =
    [ E.layout []
        (E.column [ E.width E.fill, E.height E.fill, E.padding 20 ]
            [ Input.multiline [ E.height E.fill, E.width E.fill ]
                { onChange = TextUpdated
                , text = RemoteData.withDefault "" model.text
                , label = Input.labelAbove [] (E.text "Text")
                , placeholder = Nothing
                , spellcheck = True
                }
            , Input.button []
                { onPress = Just SaveButtonClicked
                , label = E.text "Save"
                }
            ]
        )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextUpdated text ->
            ( { model | text = RemoteData.Success text }, Cmd.none )

        SaveButtonClicked ->
            case model.text of
                RemoteData.Success text ->
                    ( model, Api.save { key = model.key, text = text } OnSaveComplete )

                _ ->
                    Debug.todo "save button"

        OnLoadComplete text ->
            ( { model | text = text }, Cmd.none )

        OnSaveComplete _ ->
            ( model, Cmd.none )
