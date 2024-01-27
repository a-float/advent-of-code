const fs = require("fs");
const os = require("os");

const createEmptyNode = (name, parent) => ({
  name,
  parent: parent,
  children: {},
  files: new Map(),
});

const root = createEmptyNode("/", null);
const lines = fs
  .readFileSync("data7.txt", { encoding: "utf-8" })
  .trim()
  .split(os.EOL);

let node = root;
let i = 0;
while (i < lines.length) {
  const tokens = lines[i].split(" ");
  if (tokens[0] === "$") {
    if (tokens[1] === "cd") {
      if (tokens[2] === "/") node = root;
      else if (tokens[2] === "..") node = node.parent;
      else node = node.children[tokens[2]];
    }
    if (tokens[1] === "ls") {
      while (i + 1 < lines.length && lines[i + 1][0] !== "$") {
        const [attr, name] = lines[i + 1].split(" ");
        if (attr === "dir" && !node.children[name]) {
          node.children[name] = createEmptyNode(name, node);
        } else node.files.set(name, parseInt(attr));
        i++;
      }
    }
  }
  i++;
}

const dirSizes = [];
const getDirSizes = (node) => {
  const childrenSize = Object.values(node.children).map((x) => getDirSizes(x));
  const fileSize = Array.from(node.files.values()).reduce((a, b) => a + b, 0);
  const totalChildren = childrenSize.reduce((a, b) => a + b, 0);
  const totalSize = totalChildren + fileSize;
  dirSizes.push(totalSize);
  return totalSize;
};

getDirSizes(root);
const p1 = dirSizes.filter((x) => x < 1e5).reduce((a, b) => a + b, 0);
console.log("Part 1:", p1);

const TOTAL = 7e7;
const MIN = 3e7;

dirSizes.sort((a, b) => b - a);
const rootSize = Math.max(...dirSizes);
const p2 = Math.min(...dirSizes.filter((x) => TOTAL - rootSize + x >= MIN));
console.log("Part 2:", p2);
