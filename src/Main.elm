module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Html
import Page
import Page.Game
import Page.NewGame
import Page.NewMap
import Route
import Session
import Url



---- MODEL ----


type Model
    = NewGame Page.NewGame.Model
    | Game Page.Game.Model
    | NewMap Page.NewMap.Model
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
    | GotGameMsg Page.Game.Msg
    | GotNewGameMsg Page.NewGame.Msg
    | GotNewMapMsg Page.NewMap.Msg


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

        ( GotGameMsg subMsg, Game activeGame ) ->
            Page.Game.update subMsg activeGame
                |> updateWith Game GotGameMsg

        ( GotNewMapMsg subMsg, NewMap newMap ) ->
            Page.NewMap.update subMsg newMap
                |> updateWith NewMap GotNewMapMsg

        ( GotNewGameMsg subMsg, NewGame newGame ) ->
            Page.NewGame.update subMsg newGame
                |> updateWith NewGame GotNewGameMsg

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

        Just (Route.Game gameId playerId) ->
            Page.Game.init session gameId playerId
                |> updateWith Game GotGameMsg

        Just Route.NewMap ->
            Page.NewMap.init session
                |> updateWith NewMap GotNewMapMsg

        Nothing ->
            ( model, Cmd.none )


toSession : Model -> Session.Session
toSession model =
    case model of
        NewGame newGame ->
            newGame |> Page.NewGame.toSession

        Game activeGame ->
            activeGame |> Page.Game.toSession

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

        Game activeGame ->
            viewPage Page.Game GotGameMsg (Page.Game.view activeGame)

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

        Game activeGame ->
            Sub.map GotGameMsg (Page.Game.subscriptions activeGame)

        NewMap newMap ->
            Sub.map GotNewMapMsg (Page.NewMap.subscriptions newMap)

        Redirect _ ->
            Sub.none
