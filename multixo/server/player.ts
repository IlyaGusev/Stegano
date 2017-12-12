import * as EventEmitter from 'events';
import {Guid} from "./guid";

export class Player extends EventEmitter {
    id: string;

    constructor() {
        super();
        this.id = Guid.newGuid();
    }

    Write(json: string) {
        this.emit('write', json)
    }
}