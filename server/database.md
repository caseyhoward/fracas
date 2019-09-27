## games
id
map_id

## game_player_country_troops
game_id
player_id
country_id
troop_count

## game_country_ports -- The player can be derived from the game_player_country_troops
game_id
country_id

## players
id
game_id
name
capitol_country_id

## maps
id
name
legacy_raw_map

## countries
id
map_id
polygon_json   -- [(0,1), (0,2) ...]
water_edges    -- This is calculatable from the country polygons but it seems easier to store it for now

## adjacent_countries
map_id
country_1_id
country_2_id

## water_countries
map_id
country_id
water_id