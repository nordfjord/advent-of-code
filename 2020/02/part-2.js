const input = require('fs').readFileSync(0, 'utf-8').toString().split('\n')

/**
 * @param p {string}
 */
const Password = (p) => {
  if (!p) return null
  const [, from, to] = p.match(/^(\d+)-(\d+)/)
  const [, letter] = p.match(/(\w+):/)

  const rule = { first: Number(from), second: Number(to), char: letter }

  const [, password] = p.split(':')

  return { rule, password }
}

const xor = (a, b) => (a || b) && !(a && b)

process.stdout.write(
  '' +
    input.map(Password).filter((p) => {
      if (!p) return false
      const { password, rule } = p
      return xor(
        password[rule.first] === rule.char,
        password[rule.second] === rule.char
      )
    }).length +
    '\n'
)
