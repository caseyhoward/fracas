import * as Database from "../Database";

export const query: Database.ExecuteQuery = Database.postgres({
  user: "fracas",
  host: "localhost",
  database: "fracas_test",
  password: "abc123",
  port: 5432
});
