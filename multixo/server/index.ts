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
        if (ws.readyState === WebSocket.OPEN) {
            console.log("sent to %s: %s", player.id, json);
            ws.send(JSON.stringify(json));
            if (JSON.parse(json).action == 'close') {
                ws.terminate();
            }
        }
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

    ws.on('close', () => {
        console.log('diconnected: %s', player.id);
        manager.CloseGame(game.id);
    });

    ws.isAlive = true;
    const waitPing = () => setInterval(function ping() {
        if (!ws.isAlive) {
            ws.terminate();
            return;
        }
        ws.isAlive = false;
        ws.ping('', false, true);
        waitPing();
    }, 30000);
    ws.on('pong', () => {
        ws.isAlive = true;
    });
    waitPing();
});



server.listen(process.env.PORT || 8999, () => {
    console.log(`Server started on port ${server.address().port} :)`);
});