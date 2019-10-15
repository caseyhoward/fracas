module.exports = {
  preset: "ts-jest",
  testEnvironment: "node",
  modulePathIgnorePatterns: ["<rootDir>/build/"],
  coverageThreshold: {
    global: {
      branches: 71,
      functions: 56,
      lines: 80,
      statements: 79
    }
  }
};
