import * as InternetGameRepository from "./InternetGameRepository";
import * as TestDatabase from "../test/TestDatabase";
import * as Factories from "../test/Factories";
import * as Builders from "../test/Builders";

describe("InternetGameRepository.save", () => {
  it("save an Internet game", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});
    await InternetGameRepository.save(
      TestDatabase.query,
      Builders.internetGame(configuration.id)
    );
    const savedConfiguration = await InternetGameRepository.findById(
      TestDatabase.query
    )(configuration.id);
    expect(savedConfiguration.__typename).toEqual("InternetGame");
  });
});

describe("InternetGameRepository.saveWithoutMap", () => {
  it("save an Internet game", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});
    await InternetGameRepository.saveWithoutMap(
      TestDatabase.query,
      Builders.internetGameWithoutMap(configuration.id)
    );
    const savedConfiguration = await InternetGameRepository.findById(
      TestDatabase.query
    )(configuration.id);
    expect(savedConfiguration.__typename).toEqual("InternetGame");
  });
});
