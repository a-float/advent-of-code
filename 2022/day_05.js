const fs = require("fs");
const os = require("os");

const text = fs.readFileSync("data5.txt", { encoding: "utf-8" });

const findEndOfFirstMarker = (text, windowSize) => {

    for (let i = 0; i < text.length - windowSize + 1; i++) {
        const chunk = text.slice(i, i + windowSize);
        if(new Set(chunk).size == chunk.length){
          return i + windowSize
        }
      }
}

console.log("Part 1:", findEndOfFirstMarker(text, 4))
console.log("Part 2:", findEndOfFirstMarker(text, 14))
