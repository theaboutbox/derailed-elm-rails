UserType = GraphQL::ObjectType.define do
  name 'User'
  description 'A user with a log of events describing their feelings'
  
  field :id, !types.String
  field :name, !types.String
  field :events, !types[!EventType]
end

EventType = GraphQL::ObjectType.define do
    name 'Event'
    description "A mood and the time the user was in that mood"
    field :mood, !types.String
    field :felt_at, !types.Float
end

AddEventMutation = GraphQL::Relay::Mutation.define do
    name "AddEvent"
    description "Log a new event - the time a user had a feeling"

    input_field :user_id, !types.String
    input_field :mood, !types.String
    input_field :felt_at, !types.Float

    return_field :event, EventType

    resolve -> (obj, inputs, ctx) do 
      user = User.find(inputs[:user_id])
      event = user.events.new(mood: inputs[:mood], felt_at: Time.at(inputs[:felt_at]).to_datetime)
      user.save

      { event: event }
    end
end

MutationType = GraphQL::ObjectType.define do
    name 'Mutation'
    field :addEvent, field: AddEventMutation.field
end

QueryRoot = GraphQL::ObjectType.define do
  name 'Query'
  description 'Query root for this schema'
  
  field :user do
    type UserType
    argument :id, !types.String
    description "Find a user by ID"
    resolve -> (root, args, ctx) {
      User.find(args[:id])
    }
  end
end

Schema = GraphQL::Schema.define do
  query QueryRoot
  mutation MutationType
end
