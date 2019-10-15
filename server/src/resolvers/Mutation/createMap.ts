import * as Database from "../../Database";
import * as Map from "../../repositories/MapRepository";
import * as Graphql from "../../api/graphql";

export default async function createMap(
  executeQuery: Database.ExecuteQuery,
  input: Graphql.RequireFields<Graphql.MutationCreateMapArgs, "map">
): Promise<Graphql.Map> {
  const map = await Map.create(executeQuery, input.map);
  return Map.mapToGraphql(map);
}
