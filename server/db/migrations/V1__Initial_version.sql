CREATE TABLE maps (
  id SERIAL PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  map_json TEXT NOT NULL
);

CREATE TABLE games (
  id SERIAL PRIMARY KEY,
  map_id INT NOT NULL,
  game_json TEXT NOT NULL
);

CREATE TABLE internet_games (
  id SERIAL PRIMARY KEY,
  join_token VARCHAR(100) NOT NULL,
  map_id INTEGER NOT NULL,
  game_json TEXT
);

CREATE TABLE internet_game_players (
  id SERIAL PRIMARY KEY,
  internet_game_id INTEGER NOT NULL,
  token VARCHAR(100) NOT NULL,
  player_json TEXT
);