import Time exposing (Time)
import Html exposing (Html, program, div, button, text, li, ul, img, h1)
import Html.Attributes
import Html.Events exposing (onClick)
import Task
import Http
import Date
import Json.Decode as Decode
import Json.Encode as Encode
import Debug exposing (log)
import Json.Decode.Pipeline as Pipeline
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format)

type Mood = Nothing | Happy | Sad | Indifferent
type alias Feeling = { mood: Mood, felt_at: Time }
type alias User = { name: String, id: String, events: List Feeling }

type Msg = UserMoodIs Mood 
         | LogMood Time
         | NewEvents (Result Http.Error User)
         | SavedEvent (Result Http.Error Feeling)

type alias Model = { currentMood: Mood, currentUser: User }

main = program { init = init, view = view, update = update, subscriptions = subscriptions }

-- Initializer: Set up the initial model for the application
init: (Model, Cmd Msg)
init =
    ({ currentMood = Nothing, 
       currentUser = { name = "Cameron", id = "1", events = [] } }, queryEvents "1")

-- Make a button from a mood
moodToButton: Mood -> Html Msg 
moodToButton mood =
    let
      image_path = "images/" ++ toString mood ++ ".png"
    in
      div [ 
          onClick (UserMoodIs mood), 
          Html.Attributes.style [ ("width","300px"), ("display","inline-block") ]
      ] [
          img [ 
              Html.Attributes.src image_path,
              Html.Attributes.style [ ("width","300px") ]
          ] []
      ]

-- Serialize a Feeling as an HTML list item
feelingToListItem: Feeling -> Html Msg
feelingToListItem feeling =
    let
        date = Date.fromTime feeling.felt_at
        formatter = format config config.format.dateTime
        dateStr = formatter date 
    in      
        li [] [
            text dateStr,
            text " - ",
            text (toString feeling.mood)
        ]

-- This is the view, it turns a model into HTML
view: Model -> Html Msg
view model =
    div []
    [   div []
        [ 
        h1 [] [ text ("How are you feeling, " ++ model.currentUser.name ++ "?") ]
        , moodToButton Happy
        , moodToButton Sad
        , moodToButton Indifferent
        ],
        div []
        [ text model.currentUser.name,
          text "'s current Mood is: "
        , text (toString model.currentMood) ],
        ul [] (List.map feelingToListItem model.currentUser.events)        
    ]

-- Update the model with the user's mood at a given time
logEvent: Model -> Feeling -> Model
logEvent model feeling =
    let currentUser = model.currentUser
        events = currentUser.events
    in
        { model | currentUser = 
            { currentUser | events = feeling :: events }
        }

newFeeling: Mood -> Time -> Feeling
newFeeling mood time = { mood=mood, felt_at=time }

-- Main event handler
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserMoodIs mood -> ({model | currentMood = mood}, Task.perform LogMood Time.now)
        LogMood time -> 
            let feeling = newFeeling model.currentMood time
                newModel = logEvent model feeling
                saveRequest = saveEventRequest model.currentUser.id feeling
            in (newModel,  Http.send SavedEvent saveRequest)
        NewEvents (Ok events) -> 
            ({model | currentUser = events}, Cmd.none)
        NewEvents (Err reason) -> 
            let l = log "NewEvents Err" reason
            in (model, Cmd.none)
        SavedEvent (Ok event) ->
            let l = log "SavedEvent OK" event
            in (model, Cmd.none)
        SavedEvent (Err reason) ->
            let l = log "SavedEvent Err" reason
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
        Http.post url (Http.jsonBody body) decodeUser

saveEventRequest: String -> Feeling -> Http.Request Feeling
saveEventRequest userId feeling =
    let url = "http://localhost:3000/graphql"
        mutation = """
            mutation saveEvent($user_id: String!, $mood: String!, $felt_at: Float!) {
                addEvent(input: {user_id: $user_id, mood: $mood, felt_at: $felt_at}) {
                    event {
                        mood
                        felt_at
                    }
                }
            }
        """
        vars = Encode.object [
            ("user_id", Encode.string userId)
            ,("mood", Encode.string (toString feeling.mood))
            ,("felt_at", Encode.float (feeling.felt_at/1000))
        ]
        body = Encode.object [("query", Encode.string mutation),("variables", vars)]
    in
        Http.post url (Http.jsonBody body) decodeNewFeeling

queryEvents: String -> Cmd Msg
queryEvents userId =
    Http.send NewEvents (queryEventRequest userId)

decodeNewFeeling: Decode.Decoder Feeling
decodeNewFeeling =
    Decode.at ["data","addEvent","event"] decodeFeeling

decodeUser: Decode.Decoder User
decodeUser =
    let decoder = 
        Pipeline.decode User
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "events" (Decode.list decodeFeeling)
    in 
        Decode.at ["data","user"] <| decoder

decodeMood: Decode.Decoder Mood
decodeMood =
    let 
        strToMood moodStr = 
            case moodStr of
            "Happy" -> Decode.succeed Happy
            "Sad" -> Decode.succeed Sad
            "Indifferent" -> Decode.succeed Indifferent
            _ -> Decode.fail (moodStr ++ " is not a recognized mood.")
    in
        Decode.string 
        |> Decode.andThen strToMood
      
decodeTime: Decode.Decoder Time
decodeTime =
    let
      fixTime timeInSeconds = Decode.succeed (timeInSeconds * 1000)
    in
      Decode.float |> Decode.andThen fixTime

decodeFeeling: Decode.Decoder Feeling
decodeFeeling =
    Pipeline.decode Feeling
    |> Pipeline.required "mood" decodeMood
    |> Pipeline.required "felt_at" decodeTime