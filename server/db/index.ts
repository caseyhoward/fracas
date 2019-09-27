import { Pool } from "pg";

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

module.exports = {
  query: (text: string, params: any, callback: any) => {
    return pool.query(text, params, callback);
  }
};
