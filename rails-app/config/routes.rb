Rails.application.routes.draw do
  post '/graphql', to: 'graphql#query'
end
