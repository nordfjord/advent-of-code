const fs = require("fs")
const values = fs.readFileSync(0, "utf-8").toString().split("\n").map(Number)

for (const x of values) {
  for (const y of values) {
    for (const z of values) {
      if (x + y + z === 2020) {
        process.stdout.write(`${x * y * z}\n`)
        process.exit(0)
      }
    }
  }
}
