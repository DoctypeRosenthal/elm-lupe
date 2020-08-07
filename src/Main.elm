module Main exposing (Model, Msg(..), dayStartsAt, init, main, minuteGranularity, minutesPerHour, tickView, ticksArr, ticksPerHour, timelineView, update, view, wokeHours)

import Browser
import Browser.Dom exposing (Element)
import Html exposing (Html, div)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer exposing (Event)
import List.Extra as List



---- MODEL ----


type alias Todo =
    { startTick : TickIndex
    , endTick : TickIndex
    , title : String
    , backgroundColor : String
    }


defaultTodo =
    { startTick = 7 * 12
    , endTick = 8 * 12
    , title = "bla"
    , backgroundColor = "rgb(155,0,155)"
    }


type alias TickIndex =
    Int


type alias Model =
    { mousePos : Float
    , allTicks : List Element
    , activeTick : Maybe TickIndex
    , offsetTop : Float
    , todos : List Todo
    }


init : ( Model, Cmd Msg )
init =
    ( { mousePos = 0
      , allTicks = []
      , offsetTop = 0
      , activeTick = Nothing
      , todos = [ defaultTodo ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetTimelineOffset Float
    | SetActiveTick (Maybe Int)
    | ClickOnActiveTick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTimelineOffset offset ->
            ( { model | offsetTop = offset }, Cmd.none )

        SetActiveTick tickIndex ->
            ( { model | activeTick = tickIndex }, Cmd.none )

        ClickOnActiveTick ->
            let
                activeTick =
                    Maybe.withDefault 0 model.activeTick

                maybeTodo =
                    List.find (\x -> x.startTick <= activeTick && activeTick <= x.endTick) model.todos

                _ =
                    Debug.log "click on active Tick" <|
                        case maybeTodo of
                            Just aTodo ->
                                "Click auf ToDo " ++ aTodo.title

                            Nothing ->
                                ""
            in
            ( model, Cmd.none )



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
    wokeHours * ticksPerHour + 1


ticksArr =
    List.range 0 numberOfTicks


g : Float -> Float
g number =
    if number >= 0 then
        number

    else
        0


f : Int -> Int -> Float
f activeTick x =
    -0.00001 * toFloat (x - activeTick) ^ 4 + 1


tickView : Int -> Float -> Html Msg
tickView index scalar =
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
    in
    div
        [ Html.Attributes.class "tick"
        , Pointer.onOver (always (Just index) >> SetActiveTick)
        , onClick ClickOnActiveTick
        ]
        (tickNumberView ++ [ tickLine ])


todoView : TickIndex -> Todo -> Html Msg
todoView activeTick { startTick, endTick, backgroundColor, title } =
    div
        [ classList [ ( "todo", True ), ( "todo--hover", startTick <= activeTick && activeTick <= endTick ) ]
        , style "grid-row" (String.fromInt (startTick + 1) ++ " / " ++ String.fromInt (endTick + 1))
        , style "background" backgroundColor
        ]
        [ Html.text title ]


timelineView : Model -> Html Msg
timelineView model =
    let
        scaledTicks =
            case model.activeTick of
                Just activeTickIndex ->
                    List.map (g << f activeTickIndex) ticksArr

                Nothing ->
                    List.map (always 0) ticksArr

        tickHeights : List Float
        tickHeights =
            List.map (\scalar -> 100.0 / numberOfTicks + scalar) scaledTicks

        renderedTicks =
            List.indexedMap tickView scaledTicks

        separator =
            div
                [ class "timeline__separator"
                , style "grid-row" ("1 / " ++ String.fromInt numberOfTicks)
                ]
                []

        todosView =
            let
                tickIndex =
                    case model.activeTick of
                        Just activeTickIndex ->
                            activeTickIndex

                        Nothing ->
                            0
            in
            List.map (todoView tickIndex) model.todos
    in
    div
        [ class "timeline"
        , style "grid-template-rows" (String.join " " <| List.map (\x -> String.fromFloat x ++ "%") tickHeights)
        ]
        (renderedTicks ++ [ separator ] ++ todosView)


view : Model -> Html Msg
view model =
    div
        [ class "App"
        , Pointer.onOut (always Nothing >> SetActiveTick)
        ]
        [ div [ class "timeline__filter" ] [ timelineView model ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
