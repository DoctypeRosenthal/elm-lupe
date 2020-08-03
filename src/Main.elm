module Main exposing (Model, Msg(..), dayStartsAt, init, main, minuteGranularity, minutesPerHour, tickView, ticksArr, ticksPerHour, timelineView, update, view, wokeHours)

import Browser
import Browser.Dom exposing (Element)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events.Extra.Pointer as Pointer exposing (Event)



---- MODEL ----


type alias TickIndex =
    Int


type alias Model =
    { mousePos : Float
    , allTicks : List Element
    , activeTick : Maybe TickIndex
    , offsetTop : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { mousePos = 0
      , allTicks = []
      , offsetTop = 0
      , activeTick = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetTimelineOffset Float
    | SetActiveTick (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTimelineOffset offset ->
            ( { model | offsetTop = offset }, Cmd.none )

        SetActiveTick tickIndex ->
            ( { model | activeTick = tickIndex }, Cmd.none )



---- VIEW ----


minuteGranularity =
    5


minutesPerHour =
    60


dayStartsAt =
    8


wokeHours =
    15


ticksPerHour =
    12


numberOfTicks =
    wokeHours * ticksPerHour


ticksArr =
    List.range 0 (wokeHours * ticksPerHour + 1)


g : Float -> Float
g number =
    if number >= 0 then
        number

    else
        0


f : Int -> Int -> Float
f x activeTick =
    -0.00001 * toFloat (x - activeTick) ^ 4 + 1


tickView : Maybe Int -> Int -> Html Msg
tickView activeTick index =
    let
        myTime =
            (toFloat index * minuteGranularity) / minutesPerHour + dayStartsAt

        isFullHour =
            (toFloat <| round myTime) == myTime

        tickNumberView =
            if not isFullHour then
                []

            else
                [ div [ class "tick__number" ] [ Html.text <| String.fromFloat myTime ]
                ]

        scalar =
            case activeTick of
                Just tickIndex ->
                    (g << f index) tickIndex

                Nothing ->
                    0

        tickLineCssValue =
            String.fromFloat
                (if isFullHour then
                    1

                 else
                    scalar
                )

        tickLine =
            div
                [ class "tick__line"
                , style "opacity" tickLineCssValue
                , style "transform" <| "scaleX(" ++ tickLineCssValue ++ ")"
                ]
                []

        tickHeight =
            String.fromFloat <| 100.0 / numberOfTicks + scalar * 0.4
    in
    div
        [ Html.Attributes.class "tick"
        , style "min-height" <| tickHeight ++ "%"
        , Pointer.onOver (always (Just index) >> SetActiveTick)
        ]
        (tickNumberView ++ [ tickLine ])


timelineView : Maybe Int -> Html Msg
timelineView activeTick =
    div
        [ Html.Attributes.id "timeline" ]
        (List.map (tickView activeTick) ticksArr)


view : Model -> Html Msg
view model =
    div
        [ class "App"
        , Pointer.onOut (always Nothing >> SetActiveTick)
        ]
        [ div [ class "timeline__filter" ] [ timelineView model.activeTick ]
        ]



--
--relativePos : Pointer.Event -> ( Float, Float )
--relativePos event =
--    event.pointer.offsetPos
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
