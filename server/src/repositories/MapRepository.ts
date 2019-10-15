import * as Database from "../Database";
import * as Models from "./Models";
import * as Graphql from "../api/graphql";

export function mapToGraphql(map: Models.Map): Graphql.Map {
  return { ...map, id: map.id.toString() };
}

export async function findFirstId(
  executeQuery: Database.ExecuteQuery
): Promise<number> {
  const result = await executeQuery("SELECT * FROM maps LIMIT 1");
  return result.rows[0].id;
}

export async function findById(
  executeQuery: Database.ExecuteQuery,
  id: number
): Promise<Models.Map> {
  const result = await executeQuery("SELECT * FROM maps WHERE id = $1", [id]);
  return mapsRowToMap(result.rows[0]);
}

export async function findAll(
  executeQuery: Database.ExecuteQuery
): Promise<Models.Map[]> {
  const result = await executeQuery("SELECT * FROM maps");
  return result.rows.map(mapsRowToMap);
}

export async function create(
  executeQuery: Database.ExecuteQuery,
  newMap: Models.NewMap
): Promise<Models.Map> {
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

function mapsRowToMap(mapsRow: MapsRow | undefined): Models.Map {
  if (mapsRow) {
    const parsedJson = JSON.parse(mapsRow.map_json);
    const map = {
      id: mapsRow.id,
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

function mapInputToMapsRow(map: Models.NewMap): NewMapsRow {
  const mapJson = JSON.stringify({
    name: map.name,
    countries: map.countries,
    bodiesOfWater: map.bodiesOfWater,
    dimensions: map.dimensions
  });

  return { map_json: mapJson, name: map.name };
}
