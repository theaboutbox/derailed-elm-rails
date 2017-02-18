import Time exposing (Time, second)
import Html exposing (Html, program, div, button, text, li, ul)
import Html.Events exposing (onClick)
import Task

type alias User = { name: String, id: Int }
type Mood = Nothing | Happy | Sad | Indifferent
type alias Feeling = { mood: Mood, feltAt: Time }
type alias Events = { user: User, feelings: List Feeling}
type Msg = UserMoodIs Mood | LogMood Time

type alias Model = { currentMood: Mood, events: Events }

main = program { init = init, view = view, update = update, subscriptions = subscriptions }

-- Initializer: Set up the initial model for the application
init: (Model, Cmd Msg)
init =
    ({ currentMood = Nothing, 
       events = { user = { name = "Cameron", id = 1 }, feelings = [] } }, Cmd.none)

-- Serialize a Feeling as an HTML list item
feelingToListItem: Feeling -> Html Msg
feelingToListItem feeling =
    li [] [
        text (toString feeling.feltAt),
        text " - ",
        text (toString feeling.mood)
    ]

-- This is the view, it turns a model into HTML
view: Model -> Html Msg
view model =
    div []
    [   div []
        [ button [ onClick (UserMoodIs Happy) ] [ text "Happy" ] 
        , button [ onClick (UserMoodIs Indifferent) ] [ text "Indifferent" ] 
        , button [ onClick (UserMoodIs Sad) ] [ text "Sad" ] 
        ],
        div []
        [ text model.events.user.name,
          text "'s current Mood is: "
        , text (toString model.currentMood) ],
        ul [] (List.map feelingToListItem model.events.feelings)        
    ]

-- Update the model with the user's mood at a given time
logEvent: Model -> Time -> Model
logEvent model time =
    let newFeeling = { mood = model.currentMood, feltAt = time }
        events = model.events
        feelings = model.events.feelings
    in
        { model | events = 
            { events | feelings = newFeeling :: feelings }
        }

-- Main event handler
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserMoodIs mood -> ({model | currentMood = mood}, Task.perform LogMood Time.now)
        LogMood time -> (logEvent model time, Cmd.none)

-- Event subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none