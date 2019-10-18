import { Pool, QueryConfig, QueryResult, QueryResultRow, PoolConfig } from "pg";

export type ExecutePostgresQuery = <
  R extends QueryResultRow = any,
  I extends any[] = any[]
>(
  queryTextOrConfig: string | QueryConfig<I>,
  values?: I
) => Promise<QueryResult<R>>;

export type ExecuteQuery = ExecutePostgresQuery;

export function postgres(config: PoolConfig): Pool {
  return new Pool(config);
}
