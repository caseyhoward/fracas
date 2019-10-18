import { Pool, QueryConfig, QueryResult, QueryResultRow, PoolConfig } from "pg";

export type ExecutePostgresQuery = <
  R extends QueryResultRow = any,
  I extends any[] = any[]
>(
  queryTextOrConfig: string | QueryConfig<I>,
  values?: I
) => Promise<QueryResult<R>>;

export type ExecuteQuery = ExecutePostgresQuery;

export async function postgres(config: PoolConfig): Promise<Pool> {
  const pool = new Pool(config);
  await pool.connect();
  return pool;
}
