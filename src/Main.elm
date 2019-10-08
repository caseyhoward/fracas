module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Html
import Page
import Page.ActiveGame
import Page.EditMap
import Page.Game
import Page.NewGame
import Page.NewMap
import Route
import Session
import Url



---- MODEL ----


type Model
    = NewGame Page.NewGame.Model
    | ActiveGame Page.ActiveGame.Model
    | Game Page.Game.Model
    | EditMap Page.EditMap.Model
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
    | GotActiveGameMsg Page.ActiveGame.Msg
    | GotGameMsg Page.Game.Msg
    | GotNewGameMsg Page.NewGame.Msg
    | GotEditMapMsg Page.EditMap.Msg
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

        ( GotActiveGameMsg subMsg, ActiveGame activeGame ) ->
            Page.ActiveGame.update subMsg activeGame
                |> updateWith ActiveGame GotActiveGameMsg

        ( GotGameMsg subMsg, Game game ) ->
            Page.Game.update subMsg game
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

        Just (Route.ActiveGame activeGameId) ->
            Debug.todo ""

        -- Page.ActiveGame.init session activeGameId
        --     |> updateWith ActiveGame GotActiveGameMsg
        Just (Route.Game gameId playerId) ->
            Page.ActiveGame.init session gameId playerId
                |> updateWith ActiveGame GotActiveGameMsg

        -- Page.Game.init session gameId playerId
        --     |> updateWith Game GotGameMsg
        Just (Route.EditMap mapId) ->
            Page.EditMap.init session mapId
                |> updateWith EditMap GotEditMapMsg

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

        ActiveGame activeGame ->
            activeGame |> Page.ActiveGame.toSession

        Game game ->
            game |> Page.Game.toSession

        EditMap editMap ->
            editMap |> Page.EditMap.toSession

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

        ActiveGame activeGame ->
            viewPage Page.ActiveGame GotActiveGameMsg (Page.ActiveGame.view activeGame)

        Game game ->
            viewPage Page.Game GotGameMsg (Page.Game.view game)

        EditMap editMap ->
            viewPage Page.EditMap GotEditMapMsg (Page.EditMap.view editMap)

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

        ActiveGame activeGame ->
            Sub.map GotActiveGameMsg (Page.ActiveGame.subscriptions activeGame)

        Game game ->
            Sub.map GotGameMsg (Page.Game.subscriptions game)

        EditMap editMap ->
            Sub.map GotEditMapMsg (Page.EditMap.subscriptions editMap)

        NewMap newMap ->
            Sub.map GotNewMapMsg (Page.NewMap.subscriptions newMap)

        Redirect _ ->
            Sub.none
