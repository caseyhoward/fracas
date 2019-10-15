import * as InternetGameRepository from "./InternetGameRepository";
import * as InternetGameConfigurationRepository from "./InternetGameConfigurationRepository";
import * as TestDatabase from "../test/TestDatabase";
import * as Factories from "../test/Factories";
import * as Models from "./Models";
import * as Uuid from "../Uuid";
import * as Fixtures from "../test/Fixtures";

describe("InternetGameRepository.save", () => {
  it("save an Internet game", async () => {
    const joinToken = Uuid.generate();

    const configuration = await Factories.createInternetGameConfiguration({});

    await InternetGameRepository.save(
      TestDatabase.query,
      Fixtures.internetGame(configuration.id)
    );
    const savedConfiguration = await InternetGameRepository.findById(
      TestDatabase.query,
      configuration.id
    );
    expect(savedConfiguration.__typename).toEqual("InternetGame");
  });
});
