import {Game} from "./game";
import {Guid} from "./guid";

export class GameManager {
    games: Map<string, Game>;

    constructor() {
        this.games = new Map<string, Game>();
    }

    CreateGame() : Game {
        const game = new Game(Guid.newGuid());
        this.games.set(game.id, game);
        return game;
    }

    GetGame(gameId: string) : Game | undefined {
        return this.games.get(gameId);
    }

    GetGameWithFreeSlot() : Game {
        let freeGame = undefined;
        this.games.forEach((game: Game) => {
            if (game.players.length < 2) {
                freeGame = game;
            }
        });
        if (freeGame == undefined) {
            freeGame = this.CreateGame();
        }
        return freeGame;
    }

    CloseGame(gameId: string) {
        const game = this.GetGame(gameId);
        if (game != undefined) {
            game.Terminate();
            this.games.delete(gameId);
        }
    }
}