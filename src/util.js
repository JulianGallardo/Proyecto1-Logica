export function numberToColor(num) {
    switch (num) {
        case 2: return "#249cd1";
        case 4: return "#ec893b";
        case 8: return "#e35b89";
        case 16: return "#af4e7a";
        case 32: return "#a63e4a";
        case 64: return "#8d6ebc";
        case 128: return "#81D5CA";
        case 256: return "#708AEC"; 
        case 512: return "#3C58B4"; 
        case 1024: return "#A83896";
        case 2048: return "#822B5F"; 
        case 4096: return "#AC4339"; 
        case 8192: return "#733826"; 
        case 16384: return "#256C6F"; 
        case 32768: return "#2A3E7E"; 
        default: return "black";
    }
}


export const equalPos = (posA, posB) => posA.toString() === posB.toString();

export const valueInPos = (pos, grid, numOfColumns) => {
    return grid[pos[0] * numOfColumns + pos[1]];
}

export const posInPath = (pos, path) => {
    return path.some(posI => equalPos(posI, pos));
}

export const connectionInPath = (posA, posB, path) => {
    return path.some((pos, i) => equalPos(pos, posA) && i + 1 < path.length && equalPos(path[i + 1], posB));
}

export const isAdyacent = (posA, posB) => {
    return !equalPos(posA, posB) && Math.abs(posA[0] - posB[0]) <= 1 && Math.abs(posA[1] - posB[1]) <= 1;
}

const smallerPow2GreaterOrEqualThan = (num) => {
    const log2num = Math.floor(Math.log2(num));
    return Math.pow(2, log2num) === num ? num : Math.pow(2, log2num + 1);
}

export const joinResult = (path, grid, numOfColumns) => smallerPow2GreaterOrEqualThan(path.reduce((result, pos) => result + valueInPos(pos, grid, numOfColumns), 0));