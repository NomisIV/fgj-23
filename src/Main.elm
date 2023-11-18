module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Tuple exposing (first, second)


type alias Model =
    { playerPos : ( Float, Float )
    , inputs :
        { left : Bool
        , right : Bool
        }
    }


emptyModel : Model
emptyModel =
    { playerPos = ( centerX, centerY )
    , inputs =
        { left = False
        , right = False
        }
    }


playerSpeed : number
playerSpeed =
    500


xDirection : { a | left : Bool, right : Bool } -> number
xDirection { left, right } =
    if left == right then
        0

    else if left then
        -1

    else if right then
        1

    else
        0


type Msg
    = Frame Float
    | GotInput Input


width : number
width =
    800


height : number
height =
    800


playerSize : number
playerSize =
    50


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( emptyModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        inputs =
            model.inputs
    in
    case msg of
        Frame delta ->
            ( { model
                | playerPos =
                    ( first model.playerPos + xDirection inputs * delta * playerSpeed
                    , second model.playerPos
                    )
              }
            , Cmd.none
            )

        GotInput input ->
            case input of
                Down "ArrowLeft" ->
                    ( { model
                        | inputs = { inputs | left = True }
                      }
                    , Cmd.none
                    )

                Up "ArrowLeft" ->
                    ( { model
                        | inputs = { inputs | left = False }
                      }
                    , Cmd.none
                    )

                Down "ArrowRight" ->
                    ( { model
                        | inputs = { inputs | right = True }
                      }
                    , Cmd.none
                    )

                Up "ArrowRight" ->
                    ( { model
                        | inputs = { inputs | right = False }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


centerX : Float
centerX =
    width / 2


centerY : Float
centerY =
    height / 2


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , render model
            ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


render : Model -> Renderable
render { playerPos } =
    shapes
        [ fill Color.red
        ]
        [ rect playerPos playerSize playerSize ]


type Input
    = Down String
    | Up String


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.field "key" Decode.string |> Decode.map (Down >> GotInput))
        , onKeyUp (Decode.field "key" Decode.string |> Decode.map (Up >> GotInput))
        , onAnimationFrameDelta (\v -> Frame (v / 1000))
        ]
