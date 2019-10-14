# Fracas

An attempt to remake [this game](http://www.smozzie.com/fracas.html)

Try the early terrible version [here](http://fracas.caseyhoward.net)

## Screenshot

![gameboard](./docs/gameboard.png)

## Development

### Server

#### Tab 1

```sh
cd server && tsc -w
# Another terminal
cd server && nodemon build/index.js
```
OR

```sh
cd server && nodemon index.ts # Need ts-node installed
```

### Tab 2

```sh
cd server && docker-compose up -d # For postgres
```

### Tab 3

```sh
cd server && npm run test -- --watch
```

### Client

cd client && elm-app start


## Todo

- Mobile mouse events
- Multiplayer via internet
- Map builder
- Sound effects
- Single player (AI)
- Log
- Stats
- Random map generator
- Allies
- Multiplayer chat
- Troop count rendering (better centering and better font)
- Use elm-svg instead of elm-collage (wish I knew about that before)


## Bugs
- Don't show available moves when showing country info
- Make sure there are more countries on a map than players when starting game
- Hide "Add Player" Button when colors are used up

## Maps ideas

- https://news.ycombinator.com/item?id=21099344
- http://www-cs-students.stanford.edu/~amitp/game-programming/polygon-map-generation/
- https://www.redblobgames.com/maps/mapgen2/
- https://azgaar.github.io/Fantasy-Map-Generator/