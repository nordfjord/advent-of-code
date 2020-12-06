const fs = require('fs')
const values = fs.readFileSync(0, 'utf-8').toString().split('\n').map(Number);

for (const value of values) {
    const otherValue = values.find(other => other + value === 2020);
    if (otherValue != null) {
        process.stdout.write(`${otherValue * value}\n`);
        process.exit(0);
    }
}
process.exit(1);
