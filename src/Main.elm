module Main exposing (main)

-- import Maps.Big
-- import Maps.UnitedStates

import ActiveGame
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Collage
import Collage.Events
import Collage.Layout
import Collage.Render
import Collage.Text
import Color
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import GameMap
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Maps.Big
import Page
import Page.ActiveGame
import Page.GameConfiguration
import Random
import Random.Dict
import Random.List
import Set
import Task
import Time
import TroopCount
import Url
import ViewHelpers



---- MODEL ----


type Model
    = GameConfiguration Page.GameConfiguration.Model
    | ActiveGame Page.ActiveGame.Model


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
    Page.GameConfiguration.init
        |> Tuple.mapBoth
            (\model ->
                GameConfiguration model
            )
            (\_ ->
                Task.attempt
                    (\viewportResult ->
                        case viewportResult of
                            Ok viewport ->
                                WindowResized (round viewport.viewport.width) (round viewport.viewport.height)

                            Err _ ->
                                WindowResized 0 0
                    )
                    Browser.Dom.getViewport
            )



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



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        -- viewPage : Page.Page -> (a -> Msg) -> { title : String, content : Html.Html Msg } -> Browser.Document Msg
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
            -- { title = "Fracas", body = [ viewGameConfiguration configuringGameSettings ] }
            viewPage Page.GameConfiguration GotGameConfigurationMsg (Page.GameConfiguration.view gameConfiguration)

        ActiveGame activeGame ->
            viewPage Page.ActiveGame GotActiveGameMsg (Page.ActiveGame.view activeGame)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
