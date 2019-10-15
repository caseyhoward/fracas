import * as Database from "../../Database";
import * as Map from "../../repositories/MapRepository";
import * as Graphql from "../../api/graphql";

export default async function maps(
  executeQuery: Database.ExecuteQuery
): Promise<Graphql.Map[]> {
  const maps = await Map.findAll(executeQuery);
  return maps.map(Map.mapToGraphql);
}
