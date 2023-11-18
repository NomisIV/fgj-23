module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Debug exposing (log, toString)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Random
import Tuple exposing (first, second)


type alias Model =
    { playerPos : ( Float, Float )
    , playerVel : ( Float, Float )
    , inputs :
        { left : Bool
        , right : Bool
        , up : Bool
        }
    , platforms : List ( Float, Float )
    , platformTimer : Float
    }


emptyModel : Model
emptyModel =
    { playerPos = ( centerX, centerY )
    , playerVel = ( 0, 0 )
    , inputs =
        { left = False
        , right = False
        , up = False
        }
    , platforms = [ ( 400, 500 ) ]
    , platformTimer = spawnRate
    }


playerSpeed : number
playerSpeed =
    700


playerJumpSpeed : number
playerJumpSpeed =
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


canJump : Model -> Bool
canJump model =
    second model.playerVel >= 0


type Msg
    = Frame Float
    | GotInput InputMsg
    | SpawnPlatform Float
    | Noop


width : number
width =
    800


height : number
height =
    800


playerSize : number
playerSize =
    50


platformSize : ( number, number )
platformSize =
    ( 100, 10 )


gravitation : ( Float, Float )
gravitation =
    ( 0, 1000 )


platformSpeed : number
platformSpeed =
    100


spawnRate : Float
spawnRate =
    1


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( emptyModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


collide : Model -> Bool
collide model =
    let
        falling =
            second model.playerVel >= 0

        insidePlatform =
            List.any
                (\platform ->
                    let
                        leftCorner =
                            ( first model.playerPos, second model.playerPos + playerSize )

                        rightCorner =
                            ( first model.playerPos + playerSize, second model.playerPos + playerSize )

                        insidePlatform_ ( x, y ) =
                            x
                                >= first platform
                                && x
                                <= (first platform + first platformSize)
                                && y
                                >= second platform
                                && y
                                <= (second platform + second platformSize)
                    in
                    insidePlatform_ leftCorner || insidePlatform_ rightCorner
                )
                model.platforms
    in
    falling && insidePlatform


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        inputs =
            model.inputs

        didCollide =
            collide model
    in
    case msg of
        Frame delta ->
            ( { model
                | playerPos =
                    ( first model.playerPos + xDirection inputs * delta * playerSpeed
                    , second model.playerPos
                        + (((if not didCollide then
                                second model.playerVel

                             else
                                0
                            )
                                + platformSpeed
                           )
                            * delta
                          )
                    )
                , playerVel =
                    if didCollide then
                        if inputs.up then
                            ( first model.playerVel, -playerJumpSpeed )

                        else
                            ( first model.playerVel, 0 )

                    else
                        ( first model.playerVel + (first gravitation * delta)
                        , second model.playerVel + (second gravitation * delta)
                        )
                , platforms =
                    model.platforms
                        |> List.map (\( x, y ) -> ( x, y + platformSpeed * delta ))
                        |> List.filter (\( _, y ) -> y <= height + 100)
                , platformTimer = model.platformTimer - delta * spawnRate
              }
            , if model.platformTimer < 0 then
                Random.generate SpawnPlatform (Random.float 0 width)

              else
                Cmd.none
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

                Down UpArrow ->
                    ( { model
                        | inputs = { inputs | up = True }
                      }
                    , Cmd.none
                    )

                Up UpArrow ->
                    ( { model
                        | inputs = { inputs | up = False }
                      }
                    , Cmd.none
                    )

                NotHandled ->
                    ( model, Cmd.none )

        SpawnPlatform x ->
            ( { model
                | platforms = ( x, -10 ) :: model.platforms
                , platformTimer = spawnRate
              }
            , Cmd.none
            )

        Noop ->
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
            , shapes [ fill Color.white ] <| List.map (\pos -> rect pos (first platformSize) (second platformSize)) platforms
            ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]


type InputMsg
    = Down Input
    | Up Input
    | NotHandled


type Input
    = LeftArrow
    | RightArrow
    | UpArrow


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

            "ArrowUp" ->
                dir UpArrow

            _ ->
                NotHandled
