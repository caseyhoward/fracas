import { Pool, QueryConfig, QueryResult, QueryResultRow } from "pg";

const pool = new Pool({
  user: "fracas",
  host: "localhost",
  database: "fracas",
  password: "abc123",
  port: 5432
});

// PGHOST='localhost'
// PGUSER=process.env.USER
// PGDATABASE=process.env.USER
// PGPASSWORD=null
// PGPORT=5432

export function query<R extends QueryResultRow = any, I extends any[] = any[]>(
  queryTextOrConfig: string | QueryConfig<I>,
  values?: I
): Promise<QueryResult<R>> {
  return pool.query(queryTextOrConfig, values);
}
