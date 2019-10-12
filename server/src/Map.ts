import * as Database from "./db";

export { Map, MapInput } from "./api/graphql";
import { Map, MapInput } from "./api/graphql";

export async function findFirstId(
  executeQuery: Database.ExecuteQuery
): Promise<number> {
  const result = await executeQuery("SELECT * FROM maps LIMIT 1");
  return result.rows[0].id;
}

export interface NewMap {
  name: string;
  mapJson: string;
}

// export interface Map {
//   id: string;
//   name: string;
//   mapJson: string;
// }

export async function findById(
  executeQuery: Database.ExecuteQuery,
  id: string
): Promise<Map> {
  const result = await executeQuery("SELECT * FROM maps WHERE id = $1", [id]);
  return mapToJson(result.rows[0]);
}

export async function findAll(
  executeQuery: Database.ExecuteQuery
): Promise<Map[]> {
  try {
    const result = await executeQuery("SELECT * FROM maps");

    return result.rows.map(mapToJson);
  } catch (error) {
    console.log(error);
    throw error;
  }
}

export async function create(
  executeQuery: Database.ExecuteQuery,
  newMap: MapInput
): Promise<Map> {
  try {
    const result = await executeQuery(
      "INSERT INTO maps(name, map_json) VALUES ($1, $2) RETURNING *",
      [newMap.name, JSON.stringify(newMap)]
    );
    return mapToJson(result.rows[0]);
  } catch (error) {
    console.log(error);
    return error.toString();
  }
}

function mapToJson(mapRow: any): Map {
  console.log(mapRow);
  const mapWithoutId = JSON.parse(mapRow.map_json);
  return { ...mapWithoutId, id: mapRow.id };
}
