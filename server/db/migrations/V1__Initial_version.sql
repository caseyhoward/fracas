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
