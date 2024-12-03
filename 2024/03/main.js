const input = require('fs').readFileSync(0, 'utf-8').toString()

function p1(input) {
  const regx = /mul\((\d+),(\d+)\)/g
  let m,
    sum = 0

  do {
    m = regx.exec(input)
    if (m) {
      sum += Number(m[1]) * Number(m[2])
    }
  } while (m)
  return sum
}

function p2(input) {
  const regx = /mul\((\d+),(\d+)\)|don't\(\)|do\(\)/g
  let m,
    sum = 0,
    enabled = true

  do {
    m = regx.exec(input)
    if (m) {
      if (m[0] === "don't()") enabled = false
      else if (m[0] === 'do()') enabled = true
      else if (enabled) sum += Number(m[1]) * Number(m[2])
    }
  } while (m)
  return sum
}

console.log('Part 1:', p1(input))
console.log('Part 2:', p2(input))
