UserType = GraphQL::ObjectType.define do
  name 'User'
  description '...'
  
  field :id, !types.String
  field :name, !types.String
  field :events, !types[!EventType]
end

EventType = GraphQL::ObjectType.define do
    name 'Event'
    field :mood, !types.String
    field :felt_at, !types.Float
end

QueryRoot = GraphQL::ObjectType.define do
  name 'Query'
  description '...'
  
  field :user do
    type UserType
    argument :id, !types.String
    resolve -> (root, args, ctx) {
      User.find(args[:id])
    }
  end
end

Schema = GraphQL::Schema.define do
  query QueryRoot
end
