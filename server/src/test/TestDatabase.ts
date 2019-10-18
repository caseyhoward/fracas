import * as Database from "../Database";
import { Pool } from "pg";

export const postgresDatabase: Pool = Database.postgres({
  user: "fracas",
  host: "localhost",
  database: "fracas_test",
  password: "abc123",
  port: 5432
});

export const query = postgresDatabase.query.bind(postgresDatabase);
