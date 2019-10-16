import * as Database from "../../Database";
import * as Map from "../../repositories/MapRepository";
import * as Graphql from "../../api/graphql";

export default async function map(
  executeQuery: Database.ExecuteQuery,
  input: Graphql.QueryMapArgs
): Promise<Graphql.Map> {
  const map = await Map.findById(executeQuery, input.id);
  return Map.mapToGraphql(map);
}
