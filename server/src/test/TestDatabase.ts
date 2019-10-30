import * as Database from "../Database";
import { Pool } from "pg";

export const postgresDatabase: Pool = Database.postgres({
  user: "fracas",
  host: process.env.PGHOST || "localhost",
  database: "fracas_test",
  password: "abc123",
  port: parseInt(process.env.PGPORT || "5432", 10)
});

export const query = postgresDatabase.query.bind(postgresDatabase);
