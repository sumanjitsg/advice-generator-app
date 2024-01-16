module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, disabled, src)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Phosphor



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = ErrorPage
    | LoadingPage
    | HomePage
        { advice : Advice
        , isFetching : Bool
        , isPointerOverButton : Bool
        }


type alias Advice =
    { id : Int
    , text : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( LoadingPage, getAdvice )



-- UPDATE


type Msg
    = GotAdvice (Result Http.Error Advice)
    | GetAdvice
    | PointerOverButton
    | PointerOutButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAdvice result ->
            case result of
                Ok advice ->
                    case model of
                        HomePage state ->
                            ( HomePage
                                { state
                                    | advice = advice
                                    , isFetching = False
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( HomePage
                                { advice = advice
                                , isPointerOverButton = False
                                , isFetching = False
                                }
                            , Cmd.none
                            )

                Err _ ->
                    ( ErrorPage, Cmd.none )

        GetAdvice ->
            case model of
                HomePage state ->
                    ( HomePage { state | isFetching = True }, getAdvice )

                _ ->
                    ( model, Cmd.none )

        PointerOverButton ->
            case model of
                HomePage state ->
                    ( HomePage { state | isPointerOverButton = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PointerOutButton ->
            case model of
                HomePage state ->
                    ( HomePage { state | isPointerOverButton = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EVENT HANDLERS


onPointerOver : msg -> Attribute msg
onPointerOver msg =
    on "pointerover" (Decode.succeed msg)


onPointerOut : msg -> Attribute msg
onPointerOut msg =
    on "pointerout" (Decode.succeed msg)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        ErrorPage ->
            main_ []
                [ p [ class "error" ]
                    [ Phosphor.shieldWarning Phosphor.Regular
                        |> Phosphor.withSize 4
                        |> Phosphor.withSizeUnit "rem"
                        |> Phosphor.withClass "error"
                        |> Phosphor.toHtml []
                    , div [] [ text "Something went wrong. Check console for more info." ]
                    ]
                ]

        LoadingPage ->
            main_ []
                [ p []
                    [ Phosphor.circleNotch Phosphor.Regular
                        |> Phosphor.withSize 4
                        |> Phosphor.withSizeUnit "rem"
                        |> Phosphor.withClass "loading"
                        |> Phosphor.toHtml []
                    ]
                ]

        HomePage state ->
            main_ []
                [ div {- card container -} [ class "card-container" ]
                    [ h1 []
                        [ text ("ADVICE #" ++ String.fromInt state.advice.id) ]
                    , p []
                        [ text ("\"" ++ state.advice.text ++ "\"") ]
                    , div [ class "pattern-divider" ] []
                    ]
                , div {- button container -} [ class "btn-container" ]
                    [ button
                        [ onPointerOver PointerOverButton
                        , onPointerOut PointerOutButton
                        , onClick GetAdvice
                        , classList [ ( "pointer-over", state.isPointerOverButton ) ]
                        , disabled state.isFetching
                        ]
                        [ img [ src "../public/images/icon-dice.svg", alt "" ] [] ]
                    ]
                ]



-- HTTP
{- GET random advice from https://api.adviceslip.com/ -}


getAdvice : Cmd Msg
getAdvice =
    Http.get
        { url = "https://api.adviceslip.com/advice"
        , expect = Http.expectJson GotAdvice adviceDecoder
        }



-- DECODERS
{-
   Decode the API response into Advice type
   Example:
   {
       "slip": {
           "id": 123,
           "advice": "Don't be afraid to ask questions."
       }
   }
-}


adviceDecoder : Decoder Advice
adviceDecoder =
    Decode.field "slip" slipDecoder


slipDecoder : Decoder Advice
slipDecoder =
    Decode.map2
        Advice
        (Decode.field "id" Decode.int)
        (Decode.field "advice" Decode.string)
