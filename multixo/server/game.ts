import {Board} from './board'
import {Player} from "./player";

export class Move {
    constructor(public playerId: string, public x: number, public y:number) {}
}

export class Message {
    constructor(public gameId: string, public playerId: string, public action: string,
                public x?: number, public y?: number) {}
}

export class Game {
    players: Array<Player>;
    playerSymbols: Map<string, number>;
    board: Board;
    moves: Array<Move>;

    constructor(public id: string) {
        this.players = [];
        this.moves = [];
        this.board = new Board();
        this.playerSymbols = new Map<string, number>();
    }

    AddPlayer(player: Player) {
        this.players.push(player);
        this.playerSymbols.set(player.id, this.players.length);
        player.Write(JSON.stringify(new Message(this.id, player.id, "handshake")));
    }

    DoMove(playerId: string, x: number, y: number) {
        if (this.board.GetSymbol(x, y) != 0) {
            for (let player of this.players) {
                if (player.id == playerId ) {
                    player.Write(JSON.stringify(new Message(this.id, player.id, "turn", x, y)));
                }
            }
            return;
        }
        this.moves.push(new Move(playerId, x, y));
        this.board.SetSymbol(x, y, this.playerSymbols.get(playerId) as number);
        for (let player of this.players) {
            if (player.id == playerId ) {
                continue;
            }
            player.Write(JSON.stringify(new Message(this.id, player.id, "move", x, y)));
        }
    }

    Start() {
        const firstPlayer = this.players[0];
        firstPlayer.Write(JSON.stringify(new Message(this.id, firstPlayer.id, "turn")));
        for (let player of this.players) {
            if (player.id != firstPlayer.id) {
                player.Write(JSON.stringify(new Message(this.id, player.id, "opponent")));
            }
        }
    }

    HandleMessage(message: Message) {
        if (message.action == "move") {
            if (message.x == undefined || message.y == undefined) {
                return;
            }
            this.DoMove(message.playerId, message.x, message.y);
        }
    }

    IsFull() : boolean {
        return this.players.length == 2;
    }
}
