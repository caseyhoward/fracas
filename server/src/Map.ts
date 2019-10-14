import * as Database from "./Database";

export { Map, MapInput } from "./api/graphql";
import { Map, MapInput } from "./api/graphql";

export async function findFirstId(
  executeQuery: Database.ExecuteQuery
): Promise<number> {
  const result = await executeQuery("SELECT * FROM maps LIMIT 1");
  return result.rows[0].id;
}

export async function findById(
  executeQuery: Database.ExecuteQuery,
  id: string
): Promise<Map> {
  const result = await executeQuery("SELECT * FROM maps WHERE id = $1", [id]);
  return mapsRowToMap(result.rows[0]);
}

export async function findAll(
  executeQuery: Database.ExecuteQuery
): Promise<Map[]> {
  const result = await executeQuery("SELECT * FROM maps");
  return result.rows.map(mapsRowToMap);
}

export async function create(
  executeQuery: Database.ExecuteQuery,
  newMap: MapInput
): Promise<Map> {
  const row = mapInputToMapsRow(newMap);
  const result = await executeQuery(
    "INSERT INTO maps(name, map_json) VALUES ($1, $2) RETURNING *",
    [row.name, row.map_json]
  );
  return mapsRowToMap(result.rows[0]);
}

interface MapsRow {
  id: number;
  name: string;
  map_json: string;
}

interface NewMapsRow {
  name: string;
  map_json: string;
}

function mapsRowToMap(mapsRow: MapsRow | undefined): Map {
  if (mapsRow) {
    const parsedJson = JSON.parse(mapsRow.map_json);
    const map = {
      id: mapsRow.id.toString(),
      name: parsedJson.name,
      countries: parsedJson.countries,
      bodiesOfWater: parsedJson.bodiesOfWater,
      dimensions: parsedJson.dimensions
    };
    return map;
  } else {
    throw "Map not found";
  }
}

function mapToMapsRow(map: Map): MapsRow {
  const mapJson = JSON.stringify({
    name: map.name,
    countries: map.countries,
    bodiesOfWater: map.bodiesOfWater,
    dimensions: map.dimensions
  });

  return { id: parseInt(map.id, 10), map_json: mapJson, name: map.name };
}

function mapInputToMapsRow(map: MapInput): NewMapsRow {
  const mapJson = JSON.stringify({
    name: map.name,
    countries: map.countries,
    bodiesOfWater: map.bodiesOfWater,
    dimensions: map.dimensions
  });

  return { map_json: mapJson, name: map.name };
}
