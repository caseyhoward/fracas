import * as Database from "../../Database";
import * as MapRepository from "../../repositories/MapRepository";
import * as Graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";

export default async function gameMapResolver(
  executeQuery: Database.ExecuteQuery,
  game: Graphql.Game
): Promise<Graphql.Map> {
  const map = await MapRepository.findById(
    executeQuery,
    Models.userMapId(game.map.id)
  );
  return MapRepository.mapToGraphql(map);
}
