const input = require('fs').readFileSync(0, 'utf-8').toString()

function parse_int(input, i) {
  let str = ''
  while (input[i] >= '0' && input[i] <= '9') {
    str += input[i++]
  }
  if (!str) return
  return [Number(str), i]
}

function parse_mul(input, i) {
  let a = 0,
    b = 0
  if (input.substring(i, i + 4) !== 'mul(') return
  let res = parse_int(input, i + 4)
  if (res == null) return
  ;[a, i] = res
  if (input[i++] !== ',') return
  res = parse_int(input, i)
  if (res == null) return
  ;[b, i] = res
  if (input[i++] !== ')') return
  return [{ type: 'Mul', a, b }, i]
}

function parse_enable(input, i) {
  if (input.slice(i).startsWith('do()')) return [{ type: 'EnableMul' }, i + 4]
}

function parse_disable(input, i) {
  if (input.slice(i).startsWith("don't()"))
    return [{ type: 'DisableMul' }, i + 7]
}

function choice(parsers) {
  return (input, i) => {
    for (const parse of parsers) {
      const res = parse(input, i)
      if (res) return res
    }
    return
  }
}

function many(parser, input, i) {
  const acc = []
  while (i < input.length) {
    const res = parser(input, i)
    if (res != null) {
      acc.push(res[0])
      i = res[1]
    } else {
      i++
    }
  }
  return acc
}

function parse(input) {
  return many(choice([parse_mul, parse_enable, parse_disable]), input, 0)
}

function eval_exprs(exprs) {
  let p1 = 0,
    p2 = 0,
    enabled = true
  for (const expr of exprs) {
    switch (expr.type) {
      case 'Mul':
        const x = expr.a * expr.b
        p1 += x
        if (enabled) p2 += x
        break
      case 'EnableMul':
        enabled = true
        break
      case 'DisableMul':
        enabled = false
        break
    }
  }

  console.log('Part 1:', p1)
  console.log('Part 2:', p2)
}

eval_exprs(parse(input))
