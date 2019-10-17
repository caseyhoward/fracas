import * as Player from "../models/Player";
import * as Color from "../models/Color";
import * as Builders from "../test/Builders";

describe("Player.getNextAvailablePlayerColor", () => {
  it("gets the next color that a player isn't already using", async () => {
    const playerConfigurations: Player.PlayerConfiguration[] = [
      Builders.playerConfiguration({ id: "1", color: Color.darkGreen }),
      Builders.playerConfiguration({ id: "2", color: Color.lightYellow }),
      Builders.playerConfiguration({ id: "3", color: Color.orange })
    ];
    const color = await Player.getNextAvailablePlayerColor(
      playerConfigurations
    );
    expect(color).toEqual(Color.lightGreen);
  });
});

describe("Player.createHost", () => {
  it("gets the next color that a player isn't already using", async () => {
    const host = await Player.createHost("123");
    const expectedHost: Player.PlayerConfiguration = {
      __typename: "PlayerConfiguration",
      playerId: "123",
      name: "Host",
      color: Color.lightGreen
    };
    expect(host).toEqual(expectedHost);
  });
});
