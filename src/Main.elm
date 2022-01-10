port module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Transitions as Transition exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (classList, css, id, value)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (lazy, lazy2)
import Json.Encode as E
import Task
import Time
import UIElement exposing (toggle)


port peerConnect : String -> Cmd msg


port sendMessage : String -> Cmd msg


port connectSuccess : (String -> msg) -> Sub msg


port idReceiver : (String -> msg) -> Sub msg


port messageReceiver : (String -> msg) -> Sub msg



-- MAIN


main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { isLeft : Bool
    , myId : Maybe String
    , myJob : Maybe String
    , peerIdInput : String
    , isConnect : Bool
    , selectedPointX : String
    , selectedPointY : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model True Nothing Nothing "" False "point-x" "point-y"
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ChangeStateClicked
    | TryPeerConnect
    | TrySendMessage
    | GotId String
    | GotMessage String
    | PeerIdInputChanged String
    | ConnectSuccess String
    | PointClicked String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeStateClicked ->
            ( { model | isLeft = not model.isLeft }, sendMessage "change" )

        TryPeerConnect ->
            ( { model | peerIdInput = "" }
            , peerConnect model.peerIdInput
            )

        TrySendMessage ->
            ( model, sendMessage "My Message" )

        GotId id ->
            ( { model | myId = Just id }, Cmd.none )

        GotMessage point ->
            case model.myJob of
                Just job ->
                    if job == "Y" then
                        ( { model | selectedPointX = point }, Cmd.none )

                    else
                        ( { model | selectedPointY = point }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PeerIdInputChanged str ->
            ( { model | peerIdInput = str }, Cmd.none )

        ConnectSuccess str ->
            ( { model | isConnect = True, myJob = Just str }, Cmd.none )

        PointClicked point ->
            case model.myJob of
                Just job ->
                    if job == "X" then
                        ( { model | selectedPointX = point }, sendMessage point )

                    else
                        ( { model | selectedPointY = point }, sendMessage point )

                Nothing ->
                    ( model, Cmd.none )



-- ( { model | selectedPoint = point }, sendMessage point )
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ idReceiver GotId
        , connectSuccess ConnectSuccess
        , messageReceiver GotMessage
        ]



-- VIEW


floatDiv =
    node "float-div"


frontStyle =
    Css.batch
        [ position absolute
        , width (pct 100)
        , height (pct 100)
        , property "backface-visibility" "hidden"
        , displayFlex
        , justifyContent center
        , alignItems center
        ]


backStyle =
    Css.batch
        [ position absolute
        , width (pct 100)
        , height (pct 100)
        , property "backface-visibility" "hidden"
        , transforms [ rotateY (deg 180) ]
        , displayFlex
        , justifyContent center
        , alignItems center
        , backgroundColor (hex "#ddd")
        ]


view : Model -> Html Msg
view model =
    div []
        [ div
            [ css
                [ position absolute
                , width (px 600)
                , height (px 500)
                , top (px 50)
                , left (px 70)
                , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                , property "perspective" "1000px"
                ]
            ]
            [ if model.isConnect then
                div
                    [ css
                        [ width (pct 100)
                        , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                        , padding (px 10)
                        , boxSizing borderBox
                        ]
                    ]
                    [ div [] [ button [ onClick ChangeStateClicked ] [ text "Change" ] ]
                    ]

              else
                div
                    [ css
                        [ width (pct 100)
                        , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                        , padding (px 10)
                        , boxSizing borderBox
                        , property "user-select" "text"
                        ]
                    ]
                    [ div [ css [ property "user-select" "text" ] ] [ text (model.myId |> Maybe.withDefault "Loading...") ]
                    , div []
                        [ input
                            [ onInput PeerIdInputChanged
                            , value model.peerIdInput
                            , css [ width (px 350) ]
                            ]
                            []
                        , button [ onClick TryPeerConnect ] [ text "Connect" ]
                        ]
                    ]
            , div
                [ id "point-x"
                , case model.myJob of
                    Just "X" ->
                        onClick (PointClicked "point-x")

                    _ ->
                        classList []
                , css
                    [ position absolute
                    , top (px 300)
                    , left (px 100)
                    , width (px 80)
                    , height (px 100)
                    , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , justifyContent center
                    , alignItems center
                    , borderRadius (px 3)
                    , margin (px 20)
                    , display inlineFlex
                    , transforms [ rotateY (deg 180) ]
                    ]
                ]
                [ text "Point X" ]
            , div
                [ id "point-y"
                , case model.myJob of
                    Just "Y" ->
                        onClick (PointClicked "point-y")

                    _ ->
                        classList []
                , css
                    [ position absolute
                    , top (px 300)
                    , left (px 330)
                    , width (px 80)
                    , height (px 100)
                    , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , justifyContent center
                    , alignItems center
                    , borderRadius (px 3)
                    , margin (px 20)
                    , display inlineFlex
                    , transforms [ rotateY (deg 180) ]
                    ]
                ]
                [ text "Point Y" ]
            , div
                [ id "point-a"
                , onClick (PointClicked "point-a")
                , css
                    [ position absolute
                    , top (px 100)
                    , left (px 50)
                    , width (px 130)
                    , height (px 170)
                    , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , borderRadius (px 30)
                    , fontSize (px 25)
                    , margin (px 20)
                    , display inlineFlex
                    , transforms [ rotateZ (deg -40), rotateX (deg 60) ]
                    ]
                ]
                [ text "Point A" ]
            , div
                [ id "point-b"
                , onClick (PointClicked "point-b")
                , css
                    [ position absolute
                    , top (px 60)
                    , left (px 215)
                    , width (px 130)
                    , height (px 170)
                    , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , borderRadius (px 30)
                    , fontSize (px 25)
                    , margin (px 20)
                    , display inlineFlex
                    , transforms [ rotateZ (deg 0), rotateY (deg 30) ]
                    ]
                ]
                [ text "Point B" ]
            , div
                [ id "point-c"
                , onClick (PointClicked "point-c")
                , css
                    [ position absolute
                    , top (px 100)
                    , left (px 380)
                    , width (px 130)
                    , height (px 170)
                    , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , borderRadius (px 30)
                    , fontSize (px 25)
                    , margin (px 20)
                    , display inlineFlex
                    , transforms [ rotateZ (deg 40) ]
                    ]
                ]
                [ text "Point C" ]
            , case model.myJob of
                Just job ->
                    if job == "X" then
                        floatDiv
                            [ Attr.property "targetEleId"
                                (E.string model.selectedPointX)
                            , Attr.property "isTransitionActive" (E.bool True)
                            , css
                                [ boxShadow4 (px 1) (px 1) (px 5) (hex "#f99")
                                , backgroundColor (hex "#fff")
                                , transformStyle preserve3d
                                ]
                            ]
                            [ div [ css [ frontStyle ] ] [ text "X Front" ]
                            , div [ css [ backStyle ] ] [ text "X Back" ]
                            ]

                    else
                        floatDiv
                            [ Attr.property "targetEleId"
                                (E.string model.selectedPointY)
                            , Attr.property "isTransitionActive" (E.bool True)
                            , css
                                [ boxShadow4 (px 1) (px 1) (px 5) (hex "#99f")
                                , backgroundColor (hex "#fff")
                                , transformStyle preserve3d
                                ]
                            ]
                            [ div [ css [ frontStyle ] ] [ text "Y Front" ]
                            , div [ css [ backStyle ] ] [ text "Y Back" ]
                            ]

                Nothing ->
                    div [] []
            , case model.myJob of
                Just job ->
                    if job == "X" then
                        floatDiv
                            [ Attr.property "targetEleId"
                                (E.string model.selectedPointY)
                            , Attr.property "isTransitionActive" (E.bool True)
                            , css
                                [ boxShadow4 (px 1) (px 1) (px 5) (hex "#99f")
                                , backgroundColor (hex "#fff")
                                , transformStyle preserve3d
                                ]
                            ]
                            [ div [ css [ frontStyle ] ] [ text "Y Front" ]
                            , div [ css [ backStyle ] ] [ text "Y Back" ]
                            ]

                    else
                        floatDiv
                            [ Attr.property "targetEleId"
                                (E.string model.selectedPointX)
                            , Attr.property "isTransitionActive" (E.bool True)
                            , css
                                [ boxShadow4 (px 1) (px 1) (px 5) (hex "#f99")
                                , backgroundColor (hex "#fff")
                                , transformStyle preserve3d
                                ]
                            ]
                            [ div [ css [ frontStyle ] ] [ text "X Front" ]
                            , div [ css [ backStyle ] ] [ text "X Back" ]
                            ]

                Nothing ->
                    div [] []
            ]
        ]
