module.exports = {
  client: {
    service: {
      name: "fracas",
      localSchemaFile: "./src/schema.graphql",
      // includes: ["src/**/*.{ts,tsx}"],
      excludes: ["**/*"]
    }
  }
};
