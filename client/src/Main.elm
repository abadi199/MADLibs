module Main exposing (main)

import Api
import Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
import Pages.Edit as EditPage
import RemoteData exposing (WebData)
import Route exposing (Route)
import Session exposing (Session, navKey)
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Model
    = Redirect Session
    | Edit EditPage.Model
    | Play Session { key : String, text : String }


type Msg
    = GetNewKeyComplete (WebData String)
    | EditPageMsg EditPage.Msg
    | UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.new navKey))


toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session

        Edit editModel ->
            EditPage.toSession editModel

        Play session _ ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just Route.New ->
            ( model, Api.newKey GetNewKeyComplete )

        Just (Route.Edit key) ->
            let
                ( pageModel, pageCmd ) =
                    EditPage.init session key
            in
            ( Edit pageModel, pageCmd |> Cmd.map EditPageMsg )

        Just (Route.Play key) ->
            ( Play session { key = key, text = "" }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "MADLibs"
    , body =
        case model of
            Redirect _ ->
                blankView

            Edit pageModel ->
                EditPage.view pageModel
                    |> List.map (Html.map EditPageMsg)

            Play _ _ ->
                blankView
    }


blankView : List (Html.Html Msg)
blankView =
    []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model

        navKey =
            Session.navKey session
    in
    case ( msg, model ) of
        ( GetNewKeyComplete (RemoteData.Success newKey), _ ) ->
            let
                ( pageModel, pageCmd ) =
                    EditPage.init session newKey
            in
            ( Edit pageModel
            , Cmd.batch
                [ Navigation.pushUrl (Session.navKey session) (Route.toUrl (Route.Edit newKey))
                , pageCmd |> Cmd.map EditPageMsg
                ]
            )

        ( GetNewKeyComplete _, _ ) ->
            ( model, Cmd.none )

        ( EditPageMsg pageMsg, Edit editPageModel ) ->
            let
                ( pageModel, pageCmd ) =
                    EditPage.update pageMsg editPageModel
            in
            ( Edit pageModel, Cmd.map EditPageMsg pageCmd )

        ( EditPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( UrlRequested request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )
