import * as Database from "../../Database";
import * as MapRepository from "../../repositories/MapRepository";
import * as Graphql from "../../api/graphql";

export default async function gameMapResolver(
  executeQuery: Database.ExecuteQuery,
  game: Graphql.Game
): Promise<Graphql.Map> {
  const map = await MapRepository.findById(
    executeQuery,
    parseInt(game.map.id, 10)
  );
  return MapRepository.mapToGraphql(map);
}
