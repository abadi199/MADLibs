module Main exposing (main)

import Api
import Browser
import Browser.Events
import Browser.Navigation as Navigation
import Html
import Pages.Edit as EditPage
import Pages.Play as PlayPage
import RemoteData exposing (WebData)
import Route exposing (Route)
import Session exposing (Session, WindowSize, navKey)
import Url


main : Program WindowSize Model Msg
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
    Browser.Events.onResize (\width height -> WindowSize width height |> WindowResized)


type Model
    = Redirect Session
    | Edit EditPage.Model
    | Play PlayPage.Model


type Msg
    = GetNewKeyComplete (WebData String)
    | EditPageMsg EditPage.Msg
    | PlayPageMsg PlayPage.Msg
    | UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | WindowResized WindowSize


init : WindowSize -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init windowSize url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.new windowSize navKey))


toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session

        Edit editModel ->
            EditPage.toSession editModel

        Play playModel ->
            PlayPage.toSession playModel


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
            let
                ( pageModel, pageCmd ) =
                    PlayPage.init session key
            in
            ( Play pageModel, pageCmd |> Cmd.map PlayPageMsg )


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

            Play pageModel ->
                PlayPage.view pageModel
                    |> List.map (Html.map PlayPageMsg)
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

        ( PlayPageMsg pageMsg, Play playPageModel ) ->
            let
                ( pageModel, pageCmd ) =
                    PlayPage.update pageMsg playPageModel
            in
            ( Play pageModel, Cmd.map PlayPageMsg pageCmd )

        ( PlayPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( UrlRequested request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( WindowResized windowSize, Redirect _ ) ->
            ( Redirect (Session.resizeWindow windowSize session), Cmd.none )

        ( WindowResized windowSize, Edit pageModel ) ->
            ( EditPage.updateSession (Session.resizeWindow windowSize session) pageModel
                |> Edit
            , Cmd.none
            )

        ( WindowResized windowSize, Play pageModel ) ->
            ( PlayPage.updateSession (Session.resizeWindow windowSize session) pageModel
                |> Play
            , Cmd.none
            )
