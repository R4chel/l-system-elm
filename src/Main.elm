module Main exposing (Atom(..), Model, main)

import Browser
import Browser.Events exposing (onKeyPress)
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- CONSTANTS


width : Int
width =
    500


height : Int
height =
    500



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- ATOM


type Atom
    = A
    | B


type alias Exp =
    List Atom


type alias Rules =
    Atom -> Exp


type alias LSystem =
    { axiom : Exp
    , rules : Rules
    }



-- MODEL


type alias Model =
    { lsystem : LSystem
    , state : Exp
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    let
        lsystem =
            { axiom = [ A ]
            , rules =
                \atom ->
                    case atom of
                        A ->
                            [ A, B, A ]

                        B ->
                            [ B, B, B ]
            }
    in
    ( { lsystem = lsystem, state = lsystem.axiom }
    , Cmd.none
    )



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( 'r', "" ) ->
            Reset

        Just ( 'e', "" ) ->
            Evolve

        _ ->
            Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress keyDecoder



-- UPDATE


type Msg
    = Evolve
    | Reset
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Reset ->
            ( { model | state = model.lsystem.axiom }, Cmd.none )

        Evolve ->
            ( { model | state = List.concatMap model.lsystem.rules model.state }, Cmd.none )



-- VIEW


atomView : Atom -> Svg.Svg msg
atomView atom =
    circle
        [ cx "10"
        , cy "20"
        , r "25"
        , fill (Color.toCssString Color.blue)
        ]
        []


view : Model -> Html Msg
view model =
    svg
        [ Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height (String.fromInt height)
        , viewBox (String.join " " [ "0", "0", String.fromInt width, String.fromInt height ])
        ]
        (List.map atomView model.state)
