const fs = require('fs')

const check_desc = (a, b) => a > b && a - b <= 3
const check_asc = (a, b) => a < b && b - a <= 3

function is_safe(xs) {
  if (xs.length <= 1) return false
  const aux = (check, xs) => {
    if (xs.length <= 1) return true
    return check(xs[0], xs[1]) && aux(check, xs.slice(1))
  }
  const [a, b] = xs
  if (a === b) return false
  if (check_desc(a, b)) return aux(check_desc, xs.slice(1))
  if (check_asc(a, b)) return aux(check_asc, xs.slice(1))
  return false
}

function* remove_one(xs) {
  for (let i = 0; i < xs.length; ++i) {
    yield xs.slice(0, i).concat(xs.slice(i + 1))
  }
}

function is_safe_with_one_removed(nums) {
  for (const xs of remove_one(nums)) {
    if (is_safe(xs)) return true
  }
  return false
}

function main(nums) {
  let p1 = 0,
    p2 = 0

  for (const xs of nums) {
    if (is_safe(xs)) p1++
    if (is_safe_with_one_removed(xs)) p2++
  }

  console.log('Part 1:', p1)
  console.log('Part 2:', p2)
}

function parse() {
  return fs
    .readFileSync(0, 'utf-8')
    .toString()
    .split('\n')
    .map((x) => x.split(' ').map(Number))
}

main(parse())
