module Main exposing (main)

-- import Maps.Big
-- import Maps.UnitedStates

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Html
import Page
import Page.ActiveGame
import Page.GameConfiguration
import Route
import Session
import Task
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
        , onUrlRequest = \_ -> Noop
        , onUrlChange = \_ -> Noop
        }


init : {} -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.init key))



-- Page.GameConfiguration.init
--     |> Tuple.mapBoth
--         (\model ->
--             GameConfiguration model
--         )
--         (\_ ->
--             Task.attempt
--                 (\viewportResult ->
--                     case viewportResult of
--                         Ok viewport ->
--                             WindowResized (round viewport.viewport.width) (round viewport.viewport.height)
--                         Err _ ->
--                             WindowResized 0 0
--                 )
--                 Browser.Dom.getViewport
--         )
---- UPDATE ----


type Msg
    = WindowResized Int Int
    | Noop
    | GotActiveGameMsg Page.ActiveGame.Msg
    | GotGameConfigurationMsg Page.GameConfiguration.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotActiveGameMsg subMsg, ActiveGame activeGame ) ->
            Page.ActiveGame.update subMsg activeGame
                |> updateWith ActiveGame GotActiveGameMsg model

        ( GotGameConfigurationMsg subMsg, GameConfiguration gameConfiguration ) ->
            Page.GameConfiguration.update subMsg gameConfiguration
                |> updateWith GameConfiguration GotGameConfigurationMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
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
                |> updateWith GameConfiguration GotGameConfigurationMsg model

        Just Route.ActiveGame ->
            Page.ActiveGame.init session
                |> updateWith ActiveGame GotActiveGameMsg model

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

        Redirect session ->
            { title = "...", body = [ Html.div [] [] ] }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
