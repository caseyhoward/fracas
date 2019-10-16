module Main exposing (main)

import Browser
import Browser.Navigation
import Html
import Page
import Page.InternetGame
import Page.JoinInternetGame
import Page.LocalGame
import Page.NewGame
import Page.NewMap
import Route
import Session
import Url



---- MODEL ----


type Model
    = NewGame Page.NewGame.Model
    | LocalGame Page.LocalGame.Model
    | NewMap Page.NewMap.Model
    | InternetGame Page.InternetGame.Model
    | JoinInternetGame Page.JoinInternetGame.Model
    | Redirect Session.Session


type alias Flags =
    { viewport :
        { width : Int
        , height : Int
        }
    }


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        protocol =
            case url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        port_ =
            url.port_ |> Maybe.map (\p -> ":" ++ String.fromInt p) |> Maybe.withDefault ""

        origin =
            protocol ++ url.host ++ port_
    in
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.init key origin flags.viewport))



---- UPDATE ----


type
    Msg
    -- = ChangedRoute (Maybe Route.Route)
    = ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | GotGameMsg Page.LocalGame.Msg
    | GotNewGameMsg Page.NewGame.Msg
    | GotNewMapMsg Page.NewMap.Msg
    | GotInternetGameMsg Page.InternetGame.Msg
    | GotJoinInternetGameMsg Page.JoinInternetGame.Msg


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

        -- ( ChangedRoute route, _ ) ->
        --     changeRouteTo route model
        ( GotGameMsg subMsg, LocalGame activeGame ) ->
            Page.LocalGame.update subMsg activeGame
                |> updateWith LocalGame GotGameMsg

        ( GotNewMapMsg subMsg, NewMap newMap ) ->
            Page.NewMap.update subMsg newMap
                |> updateWith NewMap GotNewMapMsg

        ( GotNewGameMsg subMsg, NewGame newGame ) ->
            Page.NewGame.update subMsg newGame
                |> updateWith NewGame GotNewGameMsg

        ( GotInternetGameMsg subMsg, InternetGame internetGame ) ->
            Page.InternetGame.update subMsg internetGame
                |> updateWith InternetGame GotInternetGameMsg

        ( GotJoinInternetGameMsg subMsg, JoinInternetGame internetGame ) ->
            Page.JoinInternetGame.update subMsg internetGame
                |> updateWith JoinInternetGame GotJoinInternetGameMsg

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
            Page.NewGame.init session
                |> updateWith NewGame GotNewGameMsg

        Just (Route.LocalGame gameId playerId) ->
            Page.LocalGame.init session gameId playerId
                |> updateWith LocalGame GotGameMsg

        Just Route.NewMap ->
            Page.NewMap.init session
                |> updateWith NewMap GotNewMapMsg

        Just (Route.InternetGame playerToken) ->
            Page.InternetGame.init session playerToken
                |> updateWith InternetGame GotInternetGameMsg

        Just (Route.JoinInternetGame joinToken) ->
            Page.JoinInternetGame.init session joinToken
                |> updateWith JoinInternetGame GotJoinInternetGameMsg

        Nothing ->
            ( model, Cmd.none )


toSession : Model -> Session.Session
toSession model =
    case model of
        NewGame newGame ->
            newGame |> Page.NewGame.toSession

        LocalGame activeGame ->
            activeGame |> Page.LocalGame.toSession

        InternetGame internetGame ->
            internetGame |> Page.InternetGame.toSession

        JoinInternetGame internetGame ->
            internetGame |> Page.JoinInternetGame.toSession

        NewMap newMap ->
            newMap |> Page.NewMap.toSession

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
        NewGame newGame ->
            viewPage Page.NewGame GotNewGameMsg (Page.NewGame.view newGame)

        LocalGame activeGame ->
            viewPage Page.LocalGame GotGameMsg (Page.LocalGame.view activeGame)

        InternetGame internetGame ->
            viewPage Page.InternetGame GotInternetGameMsg (Page.InternetGame.view internetGame)

        JoinInternetGame joinInternetGame ->
            viewPage Page.JoinInternetGame GotJoinInternetGameMsg (Page.JoinInternetGame.view joinInternetGame)

        NewMap newMap ->
            viewPage Page.NewMap GotNewMapMsg (Page.NewMap.view newMap)

        Redirect _ ->
            { title = "Redirecting", body = [ Html.div [] [] ] }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NewGame newGame ->
            Sub.map GotNewGameMsg (Page.NewGame.subscriptions newGame)

        LocalGame activeGame ->
            Sub.map GotGameMsg (Page.LocalGame.subscriptions activeGame)

        InternetGame internetGame ->
            Sub.map GotInternetGameMsg (Page.InternetGame.subscriptions internetGame)

        JoinInternetGame _ ->
            Sub.none

        NewMap newMap ->
            Sub.map GotNewMapMsg (Page.NewMap.subscriptions newMap)

        Redirect _ ->
            Sub.none
