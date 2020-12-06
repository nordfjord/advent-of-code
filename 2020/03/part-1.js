const matrix = require('fs')
  .readFileSync(0, 'utf-8')
  .toString()
  .split('\n')
  .map((x) => x.split('').map((x) => x === '#'))
  .filter((x) => x.length)

let location = {
  x: 0,
  y: 0,
}

let treesHit = 0

function hasTreeAtLocation(location, matrix) {
  const { x, y } = location

  let pattern = matrix[y]

  while (pattern.length < x) {
    pattern = pattern.concat(pattern)
    matrix[y] = pattern
  }

  pattern[x] = pattern[x] ? 'X' : 'O'

  return pattern[x] === 'X'
}

const bottom = matrix.length - 1

while (true) {
  location.x += 3
  location.y += 1

  if (hasTreeAtLocation(location, matrix)) {
    treesHit++
  }
  if (location.y === bottom) {
    process.stdout.write(`${treesHit}\n`)
    if (process.env.DEBUG)
      process.stdout.write(
        matrix
          .map((x) =>
            x.map((x) => (x === 'O' || x === 'X' ? x : x ? '#' : '.')).join('')
          )
          .join('\n')
      )
    process.exit(0)
  }
}
