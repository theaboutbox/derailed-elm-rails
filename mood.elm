import Time exposing (Time, second)
import Html exposing (Html, program, div, button, text, li, ul)
import Html.Events exposing (onClick)
import Task
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Debug exposing (log)
import Json.Decode.Pipeline as Pipeline

type alias User = { name: String, id: String }
type Mood = Nothing | Happy | Sad | Indifferent
type alias Feeling = { mood: Mood, feltAt: Time }
type alias Events = { user: User, feelings: List Feeling}

type Msg = UserMoodIs Mood 
         | LogMood Time
         | NewEvents (Result Http.Error User)

type alias Model = { currentMood: Mood, events: Events }

main = program { init = init, view = view, update = update, subscriptions = subscriptions }

-- Initializer: Set up the initial model for the application
init: (Model, Cmd Msg)
init =
    ({ currentMood = Nothing, 
       events = { user = { name = "Cameron", id = "1" }, feelings = [] } }, queryEvents "1")

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
        NewEvents (Ok events) -> 
            let l = log "Event Data" events
            in (model, Cmd.none)
        NewEvents (Err reason) -> 
            let l = log "Error" reason
            in (model, Cmd.none)

-- Event subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP
queryEventRequest: String -> Http.Request User
queryEventRequest userId =
    let url = "http://localhost:3000/graphql"
        query = """
            { 
                user(id: "1") { 
                    name
                    id
                    events {
                        mood
                        felt_at
                    }
                } 
            }"""
        body = Encode.object [ ("query", Encode.string query) ]
    in
        Http.post url (Http.jsonBody body) decodeEvents

queryEvents: String -> Cmd Msg
queryEvents userId =
    Http.send NewEvents (queryEventRequest userId)

decodeUser: Decode.Decoder User
decodeUser = 
    Pipeline.decode User
    |> Pipeline.required "name" Decode.string
    |> Pipeline.required "id" Decode.string

decodeEvents: Decode.Decoder User
decodeEvents = 
    Decode.at ["data","user"] <| decodeUser