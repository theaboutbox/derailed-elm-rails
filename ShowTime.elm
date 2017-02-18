import Time exposing (Time, second)
import Html exposing (Html, program, div, button, text, li, ul)
import Html.Events exposing (onClick)
import Task
import Date

type alias Model = Time
type Msg = ShowTime | GetTime Time

main = program { init=init, view = view, update = update, subscriptions = subscriptions }

init : (Time, Cmd Msg)
init = (0, Task.perform GetTime Time.now)

view model =
    div []
    [   div []
        [ button [ onClick (ShowTime) ] [ text "Current Time!" ] 
        ],
        div []
        [ text (toString model) ]
    ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShowTime -> (model, Task.perform GetTime Time.now) -- model ! [Task.perform GetTime Time.now]
        GetTime time -> (time, Cmd.none) -- time ! []

subscriptions : Time -> Sub Msg
subscriptions model =
  Sub.none
