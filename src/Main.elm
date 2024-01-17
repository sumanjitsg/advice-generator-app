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
        , subscriptions = \_ -> Sub.none
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( LoadingPage, getData )


type alias Advice =
    { id : Int
    , text : String
    }



-- UPDATE


type Msg
    = ReceivedData (Result Http.Error Advice)
    | GetData
    | PointerOverButton
    | PointerOutButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedData result ->
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

        GetData ->
            case model of
                HomePage state ->
                    ( HomePage { state | isFetching = True }, getData )

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



-- VIEW


view : Model -> Html Msg
view model =
    case model of
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
                        , onClick GetData
                        , classList [ ( "pointer-over", state.isPointerOverButton ) ]
                        , disabled state.isFetching
                        ]
                        [ img [ src "../public/images/icon-dice.svg", alt "" ] [] ]
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

        ErrorPage ->
            main_ []
                [ p [ class "error" ]
                    [ Phosphor.shieldWarning Phosphor.Regular
                        |> Phosphor.withSize 4
                        |> Phosphor.withSizeUnit "rem"
                        |> Phosphor.toHtml []
                    , div [] [ text "Something went wrong. Check console for more info." ]
                    ]
                ]



-- EVENT HANDLERS


onPointerOver : msg -> Attribute msg
onPointerOver msg =
    on "pointerover" <| Decode.succeed msg


onPointerOut : msg -> Attribute msg
onPointerOut msg =
    on "pointerout" <| Decode.succeed msg



-- HTTP
{- GET random advice from https://api.adviceslip.com/ -}


getData : Cmd Msg
getData =
    Http.get
        { url = "https://api.adviceslip.com/advice"
        , expect = Http.expectJson ReceivedData responseDecoder
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


responseDecoder : Decoder Advice
responseDecoder =
    Decode.field "slip" adviceDecoder


adviceDecoder : Decoder Advice
adviceDecoder =
    Decode.map2
        Advice
        (Decode.field "id" Decode.int)
        (Decode.field "advice" Decode.string)
