require "sinatra"
require "sinatra/json"
require 'pg'
require 'json'
$conn = PG::Connection.open(:dbname => 'gem')

$conn.prepare('show_notepad', 'SELECT * FROM gem.notepads WHERE id = $1')
$conn.prepare('create_notepad', <<-SQL
  INSERT INTO gem.notepads (title, creator_token, description, contents)
  VALUES ($1, $2, $3, $4)
  RETURNING id;
SQL
)
$conn.prepare('update_notepad', <<-SQL
  UPDATE gem.notepads
  SET title = $2, description = $3, contents = $4
  WHERE id = $1;
SQL
)

# define a route that uses the helper
get '/notepads' do
  res = $conn.exec('SELECT * FROM gem.notepads;')
  json({notepads: res.to_a})
end

get '/notepads/:id' do
  res = $conn.exec_prepared('show_notepad', [params[:id]])
  json({notepad: res.to_a[0]})
end

post '/notepads' do
  content_type :json
  stuff = JSON::parse(request.body.read)
  res = $conn.exec_prepared('create_notepad', [
    stuff["title"], stuff["creator_token"], stuff["description"], stuff["contents"]
  ]);

  json({id: res[0]["id"]})
end

put '/notepads/:id' do
  # todo: maybe think about making this secure
  content_type :json
  stuff = JSON::parse(request.body.read)
  res = $conn.exec_prepared('update_notepad', [
    params["id"], stuff["title"], stuff["description"], stuff["contents"]
  ]);

  json({})
end
