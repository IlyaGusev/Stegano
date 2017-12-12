import * as express from 'express';
import * as http from 'http';
import * as WebSocket from 'ws';
import {Player} from './player';
import {GameManager} from "./manager";

const app = express();
const server = http.createServer(app);
const wss = new WebSocket.Server({ server });
const manager = new GameManager();


wss.on('connection', (ws: any) => {
    const player = new Player();
    player.on('write', (json) => {
        ws.send(JSON.stringify(json))
    });
    const game = manager.GetGameWithFreeSlot();
    game.AddPlayer(player);
    if (game.IsFull()) {
        game.Start();
    }

    ws.on('message', (message: string) => {
        console.log('received: %s', message);
        const msg = JSON.parse(message);
        let game = manager.GetGame(msg.gameId);
        if (game != undefined) {
            game.HandleMessage(msg);
        }
    });
});

server.listen(process.env.PORT || 8999, () => {
    console.log(`Server started on port ${server.address().port} :)`);
});