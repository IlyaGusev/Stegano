window.onload = function() {
    class Connection {
        constructor() {
            this.queue = []
            this.listener = null
            this.ws = new WebSocket('ws://localhost:8999');
            this.ws.onmessage = this.onMessage.bind(this);
            this.ws.onerror = this.onError.bind(this);
            this.ws.onopen = this.onOpen.bind(this);
        }

        onMessage(message) {
            const json = JSON.parse(message.data)
            console.log(json)
            if (this.listener !== null) {
                this.listener(json)
            }
        };

        onOpen() {
            this.queue.forEach(item => this.write(item))
        };

        onError() {
        }

        write(data) {
            if (this.ws.readyState === this.ws.CONNECTING) {
                this.queue.push(data)
            } else {
                this.ws.send(JSON.stringify(data))
            }
        }
    }

    var States = {
        Waiting: 0,
        Turn: 1,
        Opponent: 2,
        Win: 3,
        Lose: 4,
        Disconnected: 5
    }

    var Game = new Phaser.Game(640, 704, Phaser.CANVAS, 'example', { preload: Preload, create: Create, update: Update });
    var ConnectionInstance = null;
    var Cursors = null;
    var GameId = null;
    var PlayerId = null;
    var PlayerIdText = null;
    var Status = null;
    var StatusText = null;
    var TextStyle = {font: "20px Arial", fill: "#000000", wordWrap: true, align: "center",
        backgroundColor: "#ffffff", wordWrapWidth: 640};
    var CurrentState = States.Waiting;


    function Preload() {
        Game.load.image('cell', 'cell.jpg');
    }

    function Create() {
        Game.stage.backgroundColor = 0xffffff;
        Game.world.setBounds(0, 0, 640, 704);

        var tilesprite = Game.add.tileSprite(0, 64, 640, 640, 'cell');
        tilesprite.inputEnabled = true;
        tilesprite.events.onInputDown.add(OnInputDown);

        StatusText = Game.add.text(0, 0, "Status: ", TextStyle);

        Cursors = Game.input.keyboard.createCursorKeys();
        ConnectionInstance = new Connection();
        ConnectionInstance.listener = Listen;
    }

    function OnInputDown() {
        if (CurrentState == States.Turn) {
            var x = Game.input.x;
            var y = Game.input.y;
            AddLabel(x, y, 64, "#ff0000");
            ConnectionInstance.write({
                'gameId': GameId, 'playerId': PlayerId, action: 'move',
                'x': Math.floor(x / 64), 'y': Math.floor(y / 64)
            })
            CurrentState = States.Opponent;
        }
    }

    function Listen(json) {
        var msg = JSON.parse(json);
        if( msg.action == "handshake") {
            GameId = msg.gameId;
            PlayerId = msg.playerId;
            CurrentState = States.Waiting;
        }
        if( msg.action == "turn") {
            CurrentState = States.Turn;
        }
        if( msg.action == "opponent") {
            CurrentState = States.Opponent;
        }
        if( msg.action == "move") {
            AddLabel(msg.x * 64, msg.y * 64, 64, "#00ff00");
            CurrentState = States.Turn;
        }
    }

    function Update() {
        Game.camera.setBoundsToWorld();
        if( CurrentState == States.Waiting) {
            StatusText.setText("Status: Waiting for another player");
        }
        if( CurrentState == States.Turn) {
            StatusText.setText("Status: Your turn");
        }
        if( CurrentState == States.Opponent ) {
            StatusText.setText("Status: Opponent turn");
        }
    }

    function AddLabel(x, y, d, color) {
        var bmd = Game.make.bitmapData(64, 64);
        bmd.ctx.fillStyle = color;
        bmd.ctx.beginPath();
        bmd.ctx.arc(d/2, d/2, d/2-3, 0, 2 * Math.PI, true);
        bmd.ctx.fill();
        bmd.addToWorld(Math.floor(x/d)*d, Math.floor(y/d)*d);
    }
}