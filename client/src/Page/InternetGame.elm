module Page.InternetGame exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Browser.Events
import Colors
import Element
import Element.Background
import Game
import GameController
import Graphql.Document
import Graphql.Http
import Html
import InternetGame
import Json.Decode
import NewGame
import Ports
import RemoteData
import Session
import Time


type Msg
    = GotGame (RemoteData.RemoteData (Graphql.Http.Error Game.GameWithCurrentUser) Game.GameWithCurrentUser)
    | GameMsg Game.Msg
    | GameSaved (RemoteData.RemoteData (Graphql.Http.Error Bool) Bool)
    | ShowCountryBorderHelper
    | WindowResized Int Int
    | SubscriptionDataReceived Json.Decode.Value
      -- | SentMessage (Result (Graphql.Http.Error ()) ())
    | NewSubscriptionStatus SubscriptionStatus ()


type Model
    = Loading LoadingModel
    | Playing PlayingModel


type alias LoadingModel =
    { session : Session.Session
    , gameAndMaps : RemoteData.RemoteData (Graphql.Http.Error Game.GameWithCurrentUser) Game.GameWithCurrentUser
    , playerToken : InternetGame.PlayerToken
    }


type SubscriptionStatus
    = NotConnected
    | Connected
    | Reconnecting


type alias PlayingModel =
    { playerToken : InternetGame.PlayerToken
    , gameModel : Game.Model
    , subscriptionStatus : SubscriptionStatus
    , session : Session.Session
    }


getGame : String -> InternetGame.PlayerToken -> (RemoteData.RemoteData (Graphql.Http.Error Game.GameWithCurrentUser) Game.GameWithCurrentUser -> msg) -> Cmd msg
getGame apiUrl playerToken toMsg =
    InternetGame.get apiUrl playerToken toMsg



-- |> Graphql.Http.queryRequest apiUrl
-- |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


init : Session.Session -> InternetGame.PlayerToken -> ( Model, Cmd Msg )
init session playerToken =
    ( Loading { session = session, gameAndMaps = RemoteData.Loading, playerToken = playerToken }
    , Cmd.batch
        [ Ports.createSubscriptions (InternetGame.subscriptionDocument playerToken |> Graphql.Document.serializeSubscription)
        , getGame session.apiUrl playerToken GotGame
        ]
    )


toSession : Model -> Session.Session
toSession model =
    case model of
        Loading loadingModel ->
            loadingModel.session

        Playing playingModel ->
            playingModel.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading loadingModel ->
            case msg of
                GotGame gameRemoteData ->
                    case gameRemoteData of
                        RemoteData.Success internetGameWithUser ->
                            let
                                gameModel : Game.Model
                                gameModel =
                                    { activeGame = internetGameWithUser.game
                                    , showAvailableMoves = False
                                    , error = Nothing
                                    , playerId = internetGameWithUser.currentUserPlayerId
                                    , countryBorderHelperOutlineStatus = Game.CountryBorderHelperOutlineInactive
                                    }

                                updatedPlayingModel : PlayingModel
                                updatedPlayingModel =
                                    { gameModel = gameModel
                                    , playerToken = loadingModel.playerToken
                                    , session = loadingModel.session
                                    , subscriptionStatus = NotConnected
                                    }
                            in
                            ( Playing updatedPlayingModel
                            , Cmd.none
                            )

                        _ ->
                            ( Loading { loadingModel | gameAndMaps = gameRemoteData }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Playing playingModel ->
            case msg of
                GameMsg gameMsg ->
                    let
                        ( updatedGameModel, updatedGameCmd ) =
                            GameController.update gameMsg playingModel.gameModel

                        saveGameCmd =
                            case gameMsg of
                                Game.CountryMouseUp _ ->
                                    saveIfChanged playingModel updatedGameModel

                                Game.Pass ->
                                    saveIfChanged playingModel updatedGameModel

                                _ ->
                                    Cmd.none
                    in
                    ( Playing { playingModel | gameModel = updatedGameModel }
                    , Cmd.batch
                        [ updatedGameCmd
                        , saveGameCmd
                        ]
                    )

                ShowCountryBorderHelper ->
                    let
                        ( updatedGameModel, updatedGameCmd ) =
                            GameController.update Game.ShowCountryBorderHelper playingModel.gameModel
                    in
                    ( Playing { playingModel | gameModel = updatedGameModel }, updatedGameCmd )

                SubscriptionDataReceived newData ->
                    case Json.Decode.decodeValue (InternetGame.subscriptionDocument playingModel.playerToken |> Graphql.Document.decoder) newData of
                        Ok game ->
                            let
                                gameModel =
                                    playingModel.gameModel
                            in
                            ( Playing { playingModel | gameModel = { gameModel | activeGame = game } }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                NewSubscriptionStatus newStatus () ->
                    ( Playing { playingModel | subscriptionStatus = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


saveIfChanged : PlayingModel -> Game.Model -> Cmd Msg
saveIfChanged playingModel updatedGameModel =
    if updatedGameModel == playingModel.gameModel then
        Cmd.none

    else
        saveGame playingModel.session.apiUrl playingModel.playerToken updatedGameModel.activeGame


saveGame : String -> InternetGame.PlayerToken -> Game.Game -> Cmd Msg
saveGame apiUrl playerToken game =
    InternetGame.save apiUrl playerToken game GameSaved


view : Model -> { title : String, content : Html.Html Msg }
view model =
    case model of
        Loading _ ->
            { title = "Loading", content = Html.div [] [ Html.text "Loading" ] }

        Playing playingModel ->
            viewPlaying playingModel


viewPlaying : PlayingModel -> { title : String, content : Html.Html Msg }
viewPlaying playingModel =
    Game.view playingModel.gameModel { width = 800, height = 600 } GameMsg


layout : Element.Element Msg -> Element.Element Msg -> Html.Html Msg
layout overlay body =
    Element.layout
        [ Element.centerX
        , Element.inFront overlay
        , Element.padding 30
        , Element.Background.color (Colors.blue |> Colors.toElementColor)
        , Element.width Element.fill
        ]
        (Element.column
            [ Element.width Element.fill
            , Element.spacingXY 0 20
            , Element.Background.color (Colors.blue |> Colors.toElementColor)
            ]
            [ Element.el [ Element.width Element.fill, Element.centerX ] NewGame.title
            , body
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    [ if waitingToShowCountryHelperOutlines model then
        Time.every countryOutlineDelayMilliseconds (always ShowCountryBorderHelper)

      else
        Sub.none
    , Browser.Events.onResize (\x y -> WindowResized x y)
    , Ports.gotSubscriptionData SubscriptionDataReceived
    , Ports.socketStatusConnected (NewSubscriptionStatus Connected)
    , Ports.socketStatusReconnecting (NewSubscriptionStatus Reconnecting)
    ]
        |> Sub.batch


waitingToShowCountryHelperOutlines : Model -> Bool
waitingToShowCountryHelperOutlines model =
    case model of
        Playing playingModel ->
            case playingModel.gameModel.countryBorderHelperOutlineStatus of
                Game.CountryBorderHelperOutlineWaitingForDelay _ ->
                    True

                _ ->
                    False

        _ ->
            False


countryOutlineDelayMilliseconds : Float
countryOutlineDelayMilliseconds =
    300
