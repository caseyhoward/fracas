# Fracas

[![Build Status](https://travis-ci.org/caseyhoward/fracas.svg?branch=master)](https://travis-ci.org/caseyhoward/fracas)

An attempt to remake [this game](http://www.smozzie.com/fracas.html)

Try the early terrible version [here](http://fracas.caseyhoward.net).

## Screenshot

![gameboard](./docs/gameboard.png)

## Development

### Docker

The "easy" way to get everything running locally is to use docker-compose.

From the project root directory run

```sh
docker-compose up -d
```

## Todo

- Turn these todo's into Github issues
- Mobile mouse events
  - Hold down for country info doesn't work on mobile
- Map builder
  - It seems easier to do this then to create a random map generator 
- Sound effects
- Single player (AI)
- Log
- Stats
- Random map generator
  - https://news.ycombinator.com/item?id=21099344
  - http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/
  - https://www.redblobgames.com/maps/mapgen2/
  - https://azgaar.github.io/Fantasy-Map-Generator/
- Multiplayer chat
- Allies
- Troop count rendering (better centering and better font)
- Use elm-svg instead of elm-collage (wish I knew about that before)
  - This will allow use of lazy and keyed. It will also allow everything that svg is capable and not just what's implemented by elm-collage. This will allow filling polygons with patterns which will be nice for filling capitols. Won't need to store every single dot coordinate anymore.


## Bugs
- Don't show available moves when showing country info
- Make sure there are more countries on a map than players when starting game
- Hide "Add Player" Button when colors are used up
