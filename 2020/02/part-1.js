const input = require('fs').readFileSync(0, 'utf-8').toString().split('\n')

/**
 * @param p {string}
 */
const Password = (p) => {
  if (!p) return null
  const [, from, to] = p.match(/^(\d+)-(\d+)/)
  const [, letter] = p.match(/(\w+):/)

  const rule = { from: Number(from), to: Number(to), char: letter }

  const [, password] = p.split(':')

  return { rule, password }
}

process.stdout.write(
  '' +
    input.map(Password).filter((password) => {
      if (!password) return false
      const charCount = password.password
        .split('')
        .filter((x) => x === password.rule.char).length
      return charCount >= password.rule.from && charCount <= password.rule.to
    }).length +
    '\n'
)
