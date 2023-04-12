module Main exposing (Atom(..), Model, main)

import Browser
import Color exposing (Color)
import Html exposing (Html, button, div, text)
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


atomView : Atom -> Svg.Svg msg
atomView atom =
    circle
        [ cx "10"
        , cy "20"
        , r "25"
        , fill (Color.toCssString Color.blue)
        ]
        []



-- MODEL


type alias Model =
    List Atom



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( [ A ], Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


type Msg
    = Noop



-- VIEW


view : Model -> Html Msg
view model =
    svg
        [ Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.height (String.fromInt height)
        , viewBox (String.join " " [ "0", "0", String.fromInt width, String.fromInt height ])
        ]
        (List.map atomView model)
