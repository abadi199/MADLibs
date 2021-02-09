module Pages.Play exposing (Model, Msg, init, toSession, update, view)

import Api
import Browser.Navigation
import Button
import Element as E
import Element.Font as Font
import Element.Region as Region
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
    , show : Bool
    , showError : Bool
    }


type Msg
    = EditStoryButtonClicked
    | NewStoryButtonClicked
    | RevealButtonClicked
    | HideButtonClicked
    | OnLoadComplete (WebData String)
    | InputUpdated Int MADLib.Token


toSession : Model -> Session
toSession { session } =
    session


init : Session -> String -> ( Model, Cmd Msg )
init session key =
    ( { session = session
      , key = key
      , madlib = Result.Ok []
      , text =
            Session.getCachedStory session
                |> Maybe.map RemoteData.Success
                |> Maybe.withDefault RemoteData.NotAsked
      , show = False
      , showError = False
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
            , if model.show then
                viewMadlib model.madlib

              else
                viewInput model.madlib
            , E.column [ E.width E.fill, E.spacing 10 ]
                [ E.el [ E.centerX ]
                    (if model.show then
                        E.row [ E.spacing 10 ]
                            [ Button.button "Edit Answer" HideButtonClicked
                            , Button.button "Edit Story" EditStoryButtonClicked
                            , Button.button "New Story" NewStoryButtonClicked
                            ]

                     else
                        Button.button "Reveal Story" RevealButtonClicked
                    )
                , if model.showError && (not <| allAnswered model.madlib) then
                    E.el [ Font.color (E.rgba255 255 0 0 0.75), E.centerX ]
                        (E.text "You must fill in all the words before revealing the story")

                  else
                    E.none
                ]
            ]
        )
    ]


viewInput : Result (H.Html Msg) MADLib -> E.Element Msg
viewInput madlibResult =
    case madlibResult of
        Ok madlib ->
            E.column
                [ E.spacing 20 ]
                [ E.paragraph [] [ E.text "Fill in all the words, and then hit \"Reveal Story\" button." ]
                , madlib
                    |> List.indexedMap (MADLib.viewTokenForm InputUpdated)
                    |> List.filterMap identity
                    |> E.wrappedRow [ E.spacing 20 ]
                ]

        Err error ->
            E.html error


viewMadlib : Result (H.Html Msg) MADLib -> E.Element Msg
viewMadlib madlibResult =
    case madlibResult of
        Ok madlib ->
            madlib
                |> List.map MADLib.viewToken
                |> E.paragraph [ Font.size 26, E.spacing 20 ]

        Err error ->
            E.html error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        navKey =
            model.session |> Session.navKey
    in
    case msg of
        InputUpdated updatedIndex updatedToken ->
            ( { model
                | madlib =
                    model.madlib
                        |> Result.map
                            (List.indexedMap
                                (\index token ->
                                    if index == updatedIndex then
                                        updatedToken

                                    else
                                        token
                                )
                            )
              }
            , Cmd.none
            )

        OnLoadComplete text ->
            let
                updatedText =
                    case model.text of
                        RemoteData.Success _ ->
                            model.text

                        _ ->
                            text
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

        EditStoryButtonClicked ->
            ( model, Browser.Navigation.pushUrl navKey (Route.toUrl (Route.Edit model.key)) )

        NewStoryButtonClicked ->
            ( model, Browser.Navigation.pushUrl navKey (Route.toUrl Route.New) )

        RevealButtonClicked ->
            if allAnswered model.madlib then
                ( { model | show = True }, Cmd.none )

            else
                ( { model | showError = True }, Cmd.none )

        HideButtonClicked ->
            ( { model | show = False }, Cmd.none )


allAnswered : Result a MADLib -> Bool
allAnswered result =
    result
        |> Result.map
            (List.foldl (\token bool -> MADLib.isAnswered token && bool) True)
        |> Result.withDefault False
