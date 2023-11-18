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
    , playerVel : ( Float, Float )
    , inputs :
        { left : Bool
        , right : Bool
        }
    , platforms : List ( Float, Float )
    }


emptyModel : Model
emptyModel =
    { playerPos = ( centerX, centerY )
    , playerVel = ( centerX, centerY )
    , inputs =
        { left = False
        , right = False
        }
    , platforms = [ ( 400, 500 ) ]
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
    | GotInput InputMsg


width : number
width =
    800


height : number
height =
    800


playerSize : number
playerSize =
    50


gravitation : ( Float, Float )
gravitation =
    ( 0, -9.82 )


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
                    , second model.playerPos + (second model.playerVel * delta)
                    )
                , playerVel =
                    ( first model.playerVel + (first gravitation * delta)
                    , second model.playerVel + (second gravitation * delta)
                    )
              }
            , Cmd.none
            )

        GotInput input ->
            case input of
                Down LeftArrow ->
                    ( { model
                        | inputs = { inputs | left = True }
                      }
                    , Cmd.none
                    )

                Up LeftArrow ->
                    ( { model
                        | inputs = { inputs | left = False }
                      }
                    , Cmd.none
                    )

                Down RightArrow ->
                    ( { model
                        | inputs = { inputs | right = True }
                      }
                    , Cmd.none
                    )

                Up RightArrow ->
                    ( { model
                        | inputs = { inputs | right = False }
                      }
                    , Cmd.none
                    )

                NotHandled ->
                    ( model, Cmd.none )


centerX : Float
centerX =
    width / 2


centerY : Float
centerY =
    height / 2


view : Model -> Html Msg
view { playerPos, platforms } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , shapes [ fill Color.red ] [ rect playerPos playerSize playerSize ]
            , shapes [ fill Color.white ] <| List.map (\pos -> rect pos 100 10) platforms
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


type InputMsg
    = Down Input
    | Up Input
    | NotHandled


type Input
    = LeftArrow
    | RightArrow


subscriptions : model -> Sub Msg
subscriptions _ =
    let
        keyDecoder =
            Decode.field "key" Decode.string

        repeatDecoder =
            Decode.field "repeat" Decode.bool
    in
    Sub.batch
        [ onKeyDown (Decode.map2 (handleKey Down) repeatDecoder keyDecoder |> Decode.map GotInput)
        , onKeyUp (Decode.map2 (handleKey Up) repeatDecoder keyDecoder |> Decode.map GotInput)
        , onAnimationFrameDelta (\v -> Frame (v / 1000))
        ]


handleKey : (Input -> InputMsg) -> Bool -> String -> InputMsg
handleKey dir repeat key =
    if repeat then
        NotHandled

    else
        case key of
            "ArrowRight" ->
                dir RightArrow

            "ArrowLeft" ->
                dir LeftArrow

            _ ->
                NotHandled
