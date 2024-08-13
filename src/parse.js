'use strict'
// Convert AST from Moa code

const fail = (m, ...a) => { throw new Error(m + ': ' + a.map(string).join(' ')) }
const parse = source => {
  // operator | symbols | string | number | id | white spaces
  const regexp = /(\.\.\.[A-Za-z_]*|[!~+\-*/%<>:!=^|&]+|[()\[\]{}]|""".*?"""|"[^]*?(?<!\\)"|-?[0-9]+[0-9_]*(?:\.[0-9_]+)|[0-9A-Za-z_]+|(?:#[^\n]*|[ \n])+)/
  let offset = 0
  const tokens = source.trim().split(regexp).flatMap(code => code.length ? [{code, offset: offset+=code.length}] : [])
  let pos = 0
  const read = () => (t => t.code.match(/^[ #]/) ? tokens[++pos] || '' : t)(tokens[pos] || {code:''})
  const loop = (a, f) => pos < tokens.length ? (v => v ? loop(a.concat([v]), f) : a)(f(read())) :a
  const many = f => loop([], f)
  const until = s => many(t => (t.code.includes('\n') && ++pos, read().code === s ? (++pos, null) : parse_exp()))
  const consume = () => (t => ++pos && t)(read() || fail('out of index', pos, tokens))
  const indent = t => t.code.includes('\n') ? t.code.split('\n').at(-1).length : fail('not break line', t)
  const squash = a => a.length === 1 ? a[0] : a
  const block = a => a.length === 1 ? a[0] : a.length > 1 ? [{code: '__block'}, ...a] : a
  const parse_unit = () => {
    const suffix = t => {
      const close = tokens[pos] || {code: ''}
      const next = read()
      return close.code === '('  ? ++pos && suffix([t, ...until(')')]) :
             close.code === '['  ? ++pos && suffix([close, t, ...until(']')]) :
//             next.code  === ','  ? suffix([t, ...many(t => t.code === ',' && ++pos && consume())]) :
             next.code  === '.'  ? ++pos && suffix([next, t, consume()]) :
//             next.code  === '=>' ? ++pos && [next, t, parse_block()] :
             t
    }
    const t = consume()
    return suffix(
      t.code === '!' ? [t, parse_unit()] :
      t.code === '-' ? [t, parse_unit()] :
      t.code === '[' ? [{...t, code:'list'}, ...until(']')] :
      t.code === '(' ? squash(until(')')) :
      t.code === ':' ? [t].concat([parse_block()]) :
      t.code.startsWith('"""') ? {...t, code: '"' + t.code.slice(3, -3).replaceAll(/"/g, '\\"') + '"'} :
      t.code.match(/^[0-9]+[0-9.]*[xobe_]/) ? {...t, code: Number(t.code.replaceAll(/_/g, '')).toString()} :
      t)
  }
  const parse_exp = () => {
    const lhs = parse_unit()
    const is_op2 = s => typeof s === 'string' && s.match(/^:?[!+\-*/%<>!=^~|&]/) && !'! ~ :'.split(' ').includes(s)
    const op2s = '* ** / // % + ++ - >> << ^ & | < <= > >= == != === !== && || = := += -= *= /= %= **= =>'.split(' ')
    const priority = op => op2s.findIndex(x => x === op)
    const sort = (op, lhs, rhs) =>
      Array.isArray(rhs) && is_op2(rhs[0].code) && priority(op.code) < priority(rhs[0].code) ? [rhs[0], [op, lhs, rhs[1]], rhs[2]] :
      [op, lhs, rhs]
    return is_op2(read().code) ? (op => sort(op, lhs, parse_exp()))(consume()) : lhs
  }
  const parse_block = () => (t => t.code.includes('\n') ? parse_lines(indent(t)) : parse_line())(read())
  const parse_line = () => {
    const is_stop = s => s.includes('\n') || s === ')' || s === ']' || s === ';'
    const a = many(t => !is_stop(t.code) && parse_exp())
    const remain = []
    while (read().code === ";" && ++pos) {
      remain.push(squash(many(t => !is_stop(t.code) && parse_exp())))
    }
    return remain.length ? block([squash(a), ...remain]) : a.length === 0 ? null : a.length === 1 ? a[0] : a
  }
  const parse_lines = n => block(many(t => (t.code.includes('\n') && indent(t) === n && ++pos, parse_line())))
  return parse_lines(0)
}

module.exports = { parse }
