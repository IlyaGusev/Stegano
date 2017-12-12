export class Board {
    positions: Map<string, number>;
    constructor() {
        this.positions = new Map<string, number>();
    }

    GetSymbol(x: number, y: number) : number {
        let key = Board.formKey(x, y);
        if (!this.positions.has(key)) {
            return 0;
        }
        return this.positions.get(key) as number;
    }

    SetSymbol(x: number, y: number, symbol: number) {
        let key = Board.formKey(x, y);
        this.positions.set(key, symbol);
    }

    IsWinningTurn(x: number, y: number, symbol: number, length: number) : boolean {
        const directions = [[0, 1], [1, 0], [1, 1], [-1, 1]];
        for (let [dx, dy] of directions) {
            let count = 1;
            let i = 1;
            while (this.GetSymbol(i * dx, i * dy) == symbol) {
                count += 1;
                i += 1;
            }
            i = -1;
            while (this.GetSymbol(i * dx, i * dy) == symbol) {
                count += 1;
                i -= 1;
            }
            if (count >= length) {
                return true;
            }
        }
        return false;
    }

    static formKey( x: number, y: number ) {
        return x + ":" + y;
    }
}
