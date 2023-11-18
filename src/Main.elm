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


type alias Model =
    { playerPos : ( Float, Float ) }


type Msg
    = Frame Float
    | GotInput Input


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( { playerPos = ( centerX, centerY ) }, Cmd.none )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Frame _ ->
                        ( model, Cmd.none )

                    GotInput _ ->
                        ( model, Cmd.none )
        , subscriptions = subscriptions
        }


width : number
width =
    400


height : number
height =
    400


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
    let
        size =
            width / 3
    in
    shapes
        [ fill Color.red
        ]
        [ rect playerPos size size ]


type Input
    = Down String


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.field "key" Decode.string |> Decode.map (Down >> GotInput))
        , onAnimationFrameDelta Frame
        ]
