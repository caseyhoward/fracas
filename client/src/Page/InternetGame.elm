module Page.InternetGame exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Api.Mutation
import Api.Query
import Colors
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Game
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet
import Html
import Html.Attributes
import InternetGame
import Map
import NewGame
import Player
import RemoteData
import Session
import ViewHelpers


type Msg
    = GotGameAndMaps (RemoteData.RemoteData (Graphql.Http.Error SelectionSet) SelectionSet)
    | ChangeColorButtonClicked Int
    | ColorSelected Int Colors.Color
    | ColorSelectBackgroundClicked
    | UpdatePlayerName String
    | RemovePlayer Int
    | StartGameClicked
    | SelectMap String
    | MapUpdated (RemoteData.RemoteData (Graphql.Http.Error InternetGame.GameOrConfiguration) InternetGame.GameOrConfiguration)


type Model
    = Loading LoadingModel
    | Configuring ConfiguringModel
    | Playing PlayingModel


type alias LoadingModel =
    { session : Session.Session
    , gameAndMaps : RemoteData.RemoteData (Graphql.Http.Error SelectionSet) SelectionSet
    , playerToken : InternetGame.PlayerToken
    }


type alias ConfiguringModel =
    { session : Session.Session
    , configuration : InternetGame.Configuration
    , maps : List Map.Map
    , configureColor : Maybe Int
    , playerToken : InternetGame.PlayerToken
    }


type alias PlayingModel =
    { session : Session.Session
    , game : Game.Game
    , playerToken : InternetGame.PlayerToken
    }


type alias SelectionSet =
    { gameOrConfiguration : InternetGame.GameOrConfiguration
    , maps : List Map.Map
    }


selectionSet : InternetGame.PlayerToken -> Graphql.SelectionSet.SelectionSet SelectionSet Graphql.Operation.RootQuery
selectionSet playerToken =
    Graphql.SelectionSet.map2 SelectionSet
        (Api.Query.internetGame { playerToken = playerToken |> InternetGame.playerTokenToString } InternetGame.selectionSet)
        (Api.Query.maps Map.mapSelection)


getGameAndMaps : String -> InternetGame.PlayerToken -> (RemoteData.RemoteData (Graphql.Http.Error SelectionSet) SelectionSet -> msg) -> Cmd msg
getGameAndMaps apiUrl playerToken toMsg =
    selectionSet playerToken
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


init : Session.Session -> InternetGame.PlayerToken -> ( Model, Cmd Msg )
init session playerToken =
    ( Loading { session = session, gameAndMaps = RemoteData.Loading, playerToken = playerToken }
    , getGameAndMaps session.apiUrl playerToken GotGameAndMaps
    )


toSession : Model -> Session.Session
toSession model =
    case model of
        Loading loadingModel ->
            loadingModel.session

        Configuring configuringModel ->
            configuringModel.session

        Playing playingModel ->
            playingModel.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading loadingModel ->
            case msg of
                GotGameAndMaps gameRemoteData ->
                    case gameRemoteData of
                        RemoteData.Success gameAndMaps ->
                            case gameAndMaps.gameOrConfiguration of
                                InternetGame.InternetGameConfiguration configuration ->
                                    ( Configuring
                                        { session = loadingModel.session
                                        , configuration = configuration
                                        , configureColor = Nothing
                                        , maps = gameAndMaps.maps
                                        , playerToken = loadingModel.playerToken
                                        }
                                    , Cmd.none
                                    )

                                InternetGame.InternetGame game ->
                                    ( Playing { session = loadingModel.session, game = game, playerToken = loadingModel.playerToken }, Cmd.none )

                        _ ->
                            ( Loading { loadingModel | gameAndMaps = gameRemoteData }, Cmd.none )

                _ ->
                    Debug.todo ""

        Configuring configuringModel ->
            case msg of
                SelectMap mapId ->
                    let
                        configuration =
                            configuringModel.configuration

                        updatedConfiguringModel : InternetGame.Configuration
                        updatedConfiguringModel =
                            { configuration | mapId = mapId |> Map.Id }
                    in
                    ( Configuring { configuringModel | configuration = updatedConfiguringModel }
                    , InternetGame.updateMap configuringModel.session.apiUrl configuringModel.playerToken (Map.Id mapId) MapUpdated
                    )

                MapUpdated updatedConfiguration ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Playing playingModel ->
            Debug.todo ""


view : Model -> { title : String, content : Html.Html Msg }
view model =
    case model of
        Loading _ ->
            { title = "Loading", content = Html.div [] [ Html.text "Loading" ] }

        Configuring configuringModel ->
            viewConfiguring configuringModel

        Playing playingModel ->
            Debug.todo ""


viewConfiguring : ConfiguringModel -> { title : String, content : Html.Html Msg }
viewConfiguring configuringModel =
    let
        p =
            configuringModel.configuration.players
                |> List.map
                    (\player ->
                        case player.id of
                            Player.Id id ->
                                ( id
                                , { name = player.name, color = player.color }
                                )
                    )
                |> Dict.fromList
    in
    { title = "Configure Internet Game"
    , content =
        layout
            (playerColorSelect p configuringModel.configureColor)
            (Element.column
                [ Element.width Element.fill
                , Element.spacingXY 0 20
                , Element.Background.color (Colors.blue |> Colors.toElementColor)
                ]
                [ Element.el
                    [ Element.Background.color (Colors.gray |> Colors.toElementColor)
                    , Element.Border.rounded 10
                    , Element.centerX
                    , Element.padding 20
                    ]
                    (Element.text ("/games/internet/join/" ++ (configuringModel.configuration.joinToken |> InternetGame.joinTokenToString)))
                , Element.el [ Element.centerX ]
                    (Element.wrappedRow
                        [ Element.spacing 40, Element.centerX ]
                        [ Element.el
                            [ Element.alignTop, Element.height Element.fill, Element.width Element.fill ]
                            (playerConfiguration p)
                        , Element.el
                            [ Element.alignTop, Element.height Element.fill, Element.width Element.fill ]
                            (NewGame.mapConfiguration configuringModel.maps (Just (Map.idToString configuringModel.configuration.mapId)) SelectMap)
                        ]
                    )
                , Element.el [ Element.width Element.fill ] (NewGame.startGameButton StartGameClicked)
                ]
            )
    }


playerConfiguration : Dict.Dict Int Player.NewPlayer -> Element.Element Msg
playerConfiguration players =
    Element.column
        [ Element.spacing 20
        , Element.centerX
        , Element.Background.color (Colors.gray |> Colors.toElementColor)
        , Element.padding 20
        , Element.Border.rounded 10
        ]
        (Element.el [ Element.Font.bold ] (Element.text "Players")
            :: (players
                    |> Dict.map playerFields
                    |> Dict.values
               )
        )


playerFields : Int -> Player.NewPlayer -> Element.Element Msg
playerFields playerId player =
    Element.row [ Element.spacing 10 ]
        [ Element.row []
            [ Element.Input.text
                [ Element.width (Element.px 200)
                , Html.Attributes.id ("player-name-" ++ String.fromInt playerId) |> Element.htmlAttribute
                ]
                { onChange = UpdatePlayerName
                , text = player.name
                , placeholder = Nothing
                , label = Element.Input.labelHidden "Name"
                }
            , NewGame.colorButton player.color (ChangeColorButtonClicked playerId)
            ]

        -- , NewGame.removePlayerButton playerId
        ]


playerColorSelect : Dict.Dict Int Player.NewPlayer -> Maybe Int -> Element.Element Msg
playerColorSelect players maybePlayerId =
    case maybePlayerId of
        Just playerId ->
            case Dict.get playerId players of
                Just player ->
                    ViewHelpers.dialog
                        ColorSelectBackgroundClicked
                        [ Element.width Element.shrink, Element.height (Element.px 300) ]
                        (Element.column
                            [ Element.padding 20
                            , Element.Background.color (Colors.white |> Colors.toElementColor)
                            , Element.spacing 20
                            , Element.width (Element.px 300)
                            , Element.height Element.fill
                            ]
                            [ Element.text ("Select color for " ++ player.name)
                            , Element.wrappedRow [ Element.width Element.fill ]
                                (players
                                    |> Player.availablePlayerColors
                                    |> List.map (\color -> Element.el [ Element.height (Element.px 50) ] (NewGame.colorButton color (ColorSelected playerId color)))
                                )
                            ]
                        )

                Nothing ->
                    Element.text "Error getting player"

        Nothing ->
            Element.none


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


subscriptions model =
    Sub.none
