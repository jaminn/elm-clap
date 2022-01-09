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


type alias ToggleModel =
    { id : Int
    , isSelected : Bool
    }


type alias Model =
    { toggleUid : Int
    , toggleList : List ToggleModel
    , isLeft : Bool
    , myId : Maybe String
    , peerIdInput : String
    , isConnect : Bool
    }


addToggle :
    Bool
    -> { a | toggleUid : Int, toggleList : List ToggleModel }
    -> { a | toggleUid : Int, toggleList : List ToggleModel }
addToggle isSelected model =
    { model
        | toggleUid = model.toggleUid + 1
        , toggleList = model.toggleList ++ [ ToggleModel model.toggleUid isSelected ]
    }


removeLastToggle model =
    { model
        | toggleList =
            case List.reverse model.toggleList of
                _ :: tl ->
                    List.reverse tl

                [] ->
                    []
    }


clearToggle model =
    { model
        | toggleList =
            List.filter (not << .isSelected) model.toggleList
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        toggleList =
            List.repeat 15 False |> List.indexedMap ToggleModel
    in
    ( Model (List.length toggleList) toggleList True Nothing "" False
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ToggleClicked Int
    | PlusClicked
    | MinusClicked
    | ClearClicked
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

        ToggleClicked id ->
            ( { model
                | toggleList =
                    List.map
                        (\t ->
                            if id == t.id then
                                { t | isSelected = not t.isSelected }

                            else
                                t
                        )
                        model.toggleList
              }
            , Cmd.none
            )

        PlusClicked ->
            ( addToggle False model, Cmd.none )

        MinusClicked ->
            ( removeLastToggle model, Cmd.none )

        ClearClicked ->
            ( clearToggle model, Cmd.none )

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


toggleListView : List ToggleModel -> Html Msg
toggleListView toggleList =
    Keyed.node "div"
        [ css [ position absolute, top zero, left zero ] ]
        (List.map
            (\t ->
                ( String.fromInt t.id
                , floatDiv
                    [ Attr.property "targetEleId" (E.string ("toggle" ++ String.fromInt t.id))
                    , Attr.property "isTransitionActive" (E.bool True)
                    ]
                    [ lazy2 toggle (ToggleClicked t.id) t.isSelected ]
                )
            )
            toggleList
        )


plusButton =
    div
        [ css
            [ backgroundColor (hex "#fff")
            , width (px 30)
            , height (px 30)
            , textAlign center
            , borderRadius (px 10)
            , display inlineBlock
            ]
        ]
        [ div
            [ css
                [ fontSize (px 22)
                , transforms [ translateY (px -1) ]
                , hover [ color (hex "#333") ]
                ]
            ]
            [ text "+" ]
        ]


minusButton =
    div
        [ css
            [ backgroundColor (hex "#fff")
            , width (px 30)
            , height (px 30)
            , textAlign center
            , borderRadius (px 10)
            , display inlineBlock
            ]
        ]
        [ div
            [ css [ fontSize (px 22), transforms [ translateY (px -1) ] ] ]
            [ text "-" ]
        ]


clearButton =
    div
        [ css
            [ backgroundColor (hex "#fff")
            , height (px 30)
            , textAlign center
            , borderRadius (px 10)
            , display inlineBlock
            ]
        ]
        [ div
            [ css [ fontSize (px 18), padding (px 5), lineHeight (num 1) ] ]
            [ text "clear" ]
        ]


view2 : Model -> Html Msg
view2 model =
    div [ css [ position absolute ] ]
        [ floatDiv
            [ Attr.property "targetEleId" (E.string "toggleBox")
            , Attr.property "isTransitionActive" (E.bool True)
            , css
                [ borderRadius (px 10)
                , backgroundColor (hex "#ddd")
                , boxShadow4 (px 1) (px 1) (px 5) (hex "#999")
                , zIndex (int -1)
                ]
            ]
            []
        , div
            [ id "toggleBox"
            , css
                [ padding (px 10)
                , margin (px 20)
                ]
            ]
            [ div
                [ css [ paddingBottom (px 5) ] ]
                [ text ("토글 갯수 : " ++ String.fromInt (List.length model.toggleList)) ]
            , div
                [ css [ paddingBottom (px 5) ] ]
                [ text ("선택된 토글 갯수 : " ++ String.fromInt (List.length (List.filter .isSelected model.toggleList))) ]
            , div [ css [] ] (List.map (\t -> div [ id ("toggle" ++ String.fromInt t.id), css [ height (px 20), width (px 60), marginTop (px 10) ] ] []) model.toggleList)
            , toggleListView model.toggleList
            , div [ css [ marginTop (px 5) ] ]
                [ div [ onClick PlusClicked, css [ display inlineBlock, padding (px 3) ] ] [ plusButton ]
                , div [ onClick MinusClicked, css [ display inlineBlock, padding (px 3) ] ] [ minusButton ]
                ]
            , div [ onClick ClearClicked, css [ display inlineBlock, padding (px 3) ] ] [ clearButton ]
            ]
        ]


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
