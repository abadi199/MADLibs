module Pages.Edit exposing (Model, Msg, init, toSession, update, view)

import Browser
import Element as E
import Element.Input as Input
import Html
import Session exposing (Session)


type alias Model =
    { session : Session
    , key : String
    , text : String
    }


type Msg
    = TextUpdated String
    | SaveButtonClicked


toSession : Model -> Session
toSession { session } =
    session


init : Session -> String -> ( Model, Cmd Msg )
init session key =
    ( { session = session
      , key = key
      , text = ""
      }
    , Cmd.none
    )


view : Model -> List (Html.Html Msg)
view model =
    [ E.layout []
        (E.column [ E.width E.fill, E.height E.fill, E.padding 20 ]
            [ Input.multiline [ E.height E.fill, E.width E.fill ]
                { onChange = TextUpdated
                , text = model.text
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
            ( { model | text = text }, Cmd.none )

        SaveButtonClicked ->
            ( model, Cmd.none )
