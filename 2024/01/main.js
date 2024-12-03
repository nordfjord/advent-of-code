const fs = require('fs')

function main(input) {
  const [left, right] = input
  left.sort()
  right.sort()
  let p1 = 0
  let p2 = 0
  for (let i = 0; i < left.length; ++i) {
    p1 += Math.abs(left[i] - right[i])

    p2 += left[i] * right.filter((x) => x === left[i]).length
  }

  console.log('Part 1:', p1)
  console.log('Part 2:', p2)
}

function parse() {
  const values = fs
    .readFileSync(0, 'utf-8')
    .toString()
    .split('\n')
    .map((x) => x.split('   ').map(Number))
  const left = []
  const right = []
  for (const value of values) {
    left.push(value[0])
    right.push(value[1])
  }
  return [left, right]
}

main(parse())
