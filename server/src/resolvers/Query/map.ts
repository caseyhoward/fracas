import * as Database from "../../Database";
import * as Map from "../../repositories/MapRepository";
import * as Graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";

export default async function map(
  executeQuery: Database.ExecuteQuery,
  input: Graphql.QueryMapArgs
): Promise<Graphql.Map> {
  const map = await Map.findById(executeQuery, Models.userMapId(input.id));
  return Map.mapToGraphql(map);
}
