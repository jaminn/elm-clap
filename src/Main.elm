port module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Transitions as Transition exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css, id, value)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (lazy, lazy2)
import Json.Encode as E
import Task
import Time
import UIElement exposing (toggle)


port peerConnect : String -> Cmd msg


port sendMessage : String -> Cmd msg


port connectSuccess : (() -> msg) -> Sub msg


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
    , peerIdInput : String
    , isConnect : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model True Nothing "" False
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
    | ConnectSuccess


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

        GotMessage str ->
            if str == "change" then
                ( { model | isLeft = not model.isLeft }, Cmd.none )

            else
                ( model, Cmd.none )

        PeerIdInputChanged str ->
            ( { model | peerIdInput = str }, Cmd.none )

        ConnectSuccess ->
            ( { model | isConnect = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ idReceiver GotId
        , connectSuccess (\() -> ConnectSuccess)
        , messageReceiver GotMessage
        ]



-- VIEW


floatDiv =
    node "float-div"


view : Model -> Html Msg
view model =
    div []
        [ div
            [ css
                [ position absolute
                , width (px 500)
                , height (px 500)
                , top (px 50)
                , left (px 70)
                , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
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
                [ id "point-a"
                , css
                    [ width (px 100)
                    , height (px 100)
                    , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , justifyContent center
                    , alignItems center
                    , borderRadius (px 3)
                    , margin (px 20)
                    , display inlineFlex
                    ]
                ]
                [ text "Point A" ]
            , div
                [ id "point-b"
                , css
                    [ width (px 130)
                    , height (px 170)
                    , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , borderRadius (px 30)
                    , fontSize (px 25)
                    , margin (px 20)
                    , display inlineFlex
                    , transforms [ rotateZ (deg 45) ]
                    ]
                ]
                [ text "Point B" ]
            , floatDiv
                [ Attr.property "targetEleId"
                    (E.string
                        (if model.isLeft then
                            "point-a"

                         else
                            "point-b"
                        )
                    )
                , Attr.property "isTransitionActive" (E.bool True)
                , css
                    [ boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , backgroundColor (hex "eee")
                    ]
                ]
                [ text "Float Box1" ]
            , floatDiv
                [ Attr.property "targetEleId"
                    (E.string
                        (if model.isLeft then
                            "point-b"

                         else
                            "point-a"
                        )
                    )
                , Attr.property "isTransitionActive" (E.bool True)
                , css
                    [ boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                    , displayFlex
                    , justifyContent center
                    , alignItems center
                    , backgroundColor (hex "eee")
                    ]
                ]
                [ text "Float Box2" ]
            ]
        ]
