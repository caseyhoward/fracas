module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Html
import Page
import Page.ActiveGame
import Page.GameConfiguration
import Route
import Session
import Url



---- MODEL ----


type Model
    = GameConfiguration Page.GameConfiguration.Model
    | ActiveGame Page.ActiveGame.Model
    | Redirect Session.Session


main : Program {} Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }


init : {} -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.init key))



---- UPDATE ----


type Msg
    = ChangedRoute (Maybe Route.Route)
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | GotActiveGameMsg Page.ActiveGame.Msg
    | GotGameConfigurationMsg Page.GameConfiguration.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotActiveGameMsg subMsg, ActiveGame activeGame ) ->
            Page.ActiveGame.update subMsg activeGame
                |> updateWith ActiveGame GotActiveGameMsg

        ( GotGameConfigurationMsg subMsg, GameConfiguration gameConfiguration ) ->
            Page.GameConfiguration.update subMsg gameConfiguration
                |> updateWith GameConfiguration GotGameConfigurationMsg

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Just Route.ConfiguringGame ->
            Page.GameConfiguration.init session
                |> updateWith GameConfiguration GotGameConfigurationMsg

        Just (Route.ActiveGame activeGameId) ->
            Page.ActiveGame.init session activeGameId
                |> updateWith ActiveGame GotActiveGameMsg

        Nothing ->
            -- ( NotFound windowSize, Cmd.none )
            ( model, Cmd.none )


toSession : Model -> Session.Session
toSession model =
    case model of
        GameConfiguration gameConfiguration ->
            gameConfiguration |> Page.GameConfiguration.toSession

        ActiveGame activeGame ->
            activeGame |> Page.ActiveGame.toSession

        Redirect session ->
            session



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        GameConfiguration gameConfiguration ->
            viewPage Page.GameConfiguration GotGameConfigurationMsg (Page.GameConfiguration.view gameConfiguration)

        ActiveGame activeGame ->
            viewPage Page.ActiveGame GotActiveGameMsg (Page.ActiveGame.view activeGame)

        Redirect _ ->
            { title = "...", body = [ Html.div [] [] ] }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameConfiguration gameConfiguration ->
            Sub.map GotGameConfigurationMsg (Page.GameConfiguration.subscriptions gameConfiguration)

        ActiveGame activeGame ->
            Sub.map GotActiveGameMsg (Page.ActiveGame.subscriptions activeGame)

        Redirect _ ->
            Sub.none
