require "sinatra"
require "sinatra/json"
require 'pg'
require 'json'

current_dir = Dir.pwd
Dir["#{current_dir}/models/*.rb"].each { |file| require file }

# define a route that uses the helper
get '/notepads' do
  json({notepads: Notepad.all.to_a})
end

get '/notepads/:id' do
  json({notepad: Notepad.find(params[:id])})
end

post '/notepads' do
  content_type :json
  stuff = JSON::parse(request.body.read)
  new_notepad = Notepad.create(stuff)
  json({id: new_notepad.id})
end

put '/notepads/:id' do
  # todo: maybe think about making this secure
  content_type :json
  stuff = JSON::parse(request.body.read)

  Notepad.update(params["id"], stuff)

  json({})
end
