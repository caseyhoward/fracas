import { Pool, QueryConfig, QueryResult, QueryResultRow } from "pg";

// PGHOST='localhost'
// PGUSER=process.env.USER
// PGDATABASE=process.env.USER
// PGPASSWORD=null
// PGPORT=5432

const pool = new Pool({
  user: "fracas",
  host: "localhost",
  database: "fracas",
  password: "abc123",
  port: 5432
});

export type ExecuteQuery = <
  R extends QueryResultRow = any,
  I extends any[] = any[]
>(
  queryTextOrConfig: string | QueryConfig<I>,
  values?: I
) => Promise<QueryResult<R>>;

export const query: ExecuteQuery = <
  R extends QueryResultRow = any,
  I extends any[] = any[]
>(
  queryTextOrConfig: string | QueryConfig<I>,
  values?: I
): Promise<QueryResult<R>> => {
  return pool.query(queryTextOrConfig, values);
};

