module.exports = {
  preset: "ts-jest",
  testEnvironment: "node",
  rootDir: ".",
  modulePathIgnorePatterns: ["<rootDir>/build/"],
  coverageThreshold: {
    global: {
      branches: 66,
      functions: 56,
      lines: 80,
      statements: 79
    }
  }
};
