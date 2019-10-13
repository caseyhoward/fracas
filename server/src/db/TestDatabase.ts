import { Pool, QueryConfig, QueryResult, QueryResultRow } from "pg";
import * as Database from "../Database";

// PGHOST='localhost'
// PGUSER=process.env.USER
// PGDATABASE=process.env.USER
// PGPASSWORD=null
// PGPORT=5432

const pool = new Pool({
  user: "fracas",
  host: "localhost",
  database: "fracas-test",
  password: "abc123",
  port: 5432
});

export const query: Database.ExecuteQuery = <
  R extends QueryResultRow = any,
  I extends any[] = any[]
>(
  queryTextOrConfig: string | QueryConfig<I>,
  values?: I
): Promise<QueryResult<R>> => {
  return pool.query(queryTextOrConfig, values);
};
