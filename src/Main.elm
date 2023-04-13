module Main exposing (Atom(..), Model, main)

import Browser
import Browser.Events exposing (onKeyPress)
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html, button, div)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes as SvgAttr exposing (..)



-- CONSTANTS


width : Int
width =
    1000


height : Int
height =
    1000



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


type Constant
    = F
    | Plus
    | Minus


type Symbol
    = Atom Atom
    | Constant Constant


type alias Exp =
    List Symbol


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
            { axiom = [ Atom A ]
            , rules = rules
            }
    in
    ( { lsystem = lsystem, state = lsystem.axiom }
    , Cmd.none
    )


rules : Rules
rules atom =
    case atom of
        A ->
            [ Constant Plus, Atom B, Constant Minus, Atom A, Constant F, Atom A, Constant Minus, Constant F, Atom B, Constant Plus ]

        B ->
            [ Constant Minus, Atom A, Constant F, Constant Plus, Atom B, Constant F, Atom B, Constant Plus, Constant F, Atom A, Constant Minus ]



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
            ( { model
                | state =
                    List.concatMap
                        (\symbol ->
                            case symbol of
                                Atom atom ->
                                    model.lsystem.rules atom

                                (Constant _) as c ->
                                    [ c ]
                        )
                        model.state
              }
            , Cmd.none
            )



-- VIEW


lineLength : Float
lineLength =
    5


executeCommands : Exp -> List (Svg.Svg msg)
executeCommands exp =
    let
        result =
            List.foldl
                (\symbol acc ->
                    case symbol of
                        Atom _ ->
                            acc

                        Constant F ->
                            let
                                newX =
                                    acc.x + lineLength * cos acc.direction
                            in
                            let
                                newY =
                                    acc.y + lineLength * sin acc.direction
                            in
                            let
                                newPath =
                                    Svg.line [ x1 (String.fromFloat acc.x), y1 (String.fromFloat acc.y), x2 (String.fromFloat newX), y2 (String.fromFloat newY), stroke (Color.toCssString Color.blue) ] []
                            in
                            { x = newX
                            , y = newY
                            , direction = acc.direction
                            , msgs = newPath :: acc.msgs
                            }

                        Constant Plus ->
                            { acc | direction = acc.direction + pi / 2 }

                        Constant Minus ->
                            { acc | direction = acc.direction - pi / 2 }
                )
                { x = 0, y = toFloat height / 2, direction = 0, msgs = [] }
                exp
    in
    result.msgs


view : Model -> Html Msg
view model =
    svg
        [ SvgAttr.width (String.fromInt width)
        , SvgAttr.height (String.fromInt height)
        , viewBox (String.join " " [ "0", "0", String.fromInt width, String.fromInt height ])
        ]
        (executeCommands model.state)
