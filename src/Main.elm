module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
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
        { playerX : Float }
    }


emptyModel : Model
emptyModel =
    { playerPos = ( centerX, centerY )
    , inputs =
        { playerX = 0
        }
    }


playerSpeed : number
playerSpeed =
    500


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
        , update =
            \msg model ->
                case msg of
                    Frame delta ->
                        ( { model
                            | playerPos =
                                ( first model.playerPos + model.inputs.playerX * delta * playerSpeed
                                , second model.playerPos
                                )
                          }
                        , Cmd.none
                        )

                    GotInput input ->
                        case input of
                            Down "ArrowLeft" ->
                                ( { model
                                    | inputs = { playerX = -1 }
                                  }
                                , Cmd.none
                                )

                            Down "ArrowRight" ->
                                ( { model
                                    | inputs = { playerX = 1 }
                                  }
                                , Cmd.none
                                )

                            _ ->
                                ( model, Cmd.none )
        , subscriptions = subscriptions
        }


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


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.field "key" Decode.string |> Decode.map (Down >> GotInput))
        , onAnimationFrameDelta (\v -> Frame (v / 1000))
        ]
