const matrix = require('fs')
  .readFileSync(0, 'utf-8')
  .toString()
  .split('\n')
  .map((x) => x.split('').map((x) => x === '#'))
  .filter((x) => x.length)

function hasTreeAtLocation(location, matrix) {
  const { x, y } = location

  let pattern = matrix[y]

  while (pattern.length <= x) {
    pattern = pattern.concat(pattern)
    matrix[y] = pattern
  }

  pattern[x] = pattern[x] ? 'X' : 'O'

  return pattern[x] === 'X'
}

const bottom = matrix.length - 1

const traversalPatterns = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
]

function logResult(treesHit, matrix, [x, y]) {
  process.stdout.write(`${treesHit}\n`)
  if (process.env.DEBUG)
    process.stdout.write(
      matrix
        .map((x) =>
          x.map((x) => (x === 'O' || x === 'X' ? x : x ? '#' : '.')).join('')
        )
        .join('\n') + '\n'
    )
}

function traverse(matrix, [x, y]) {
  const location = { x: 0, y: 0 }
  let treesHit = 0
  while (true) {
    location.x += x
    location.y += y

    if (hasTreeAtLocation(location, matrix)) {
      treesHit++
    }
    if (location.y == bottom) {
      logResult(treesHit, matrix, [x, y])
      return treesHit
    }
  }
}

const multiply = (a, b) => a * b
process.stdout.write(
  traversalPatterns
    .map((pattern) => traverse(matrix, pattern))
    .reduce(multiply, 1) + '\n'
)
