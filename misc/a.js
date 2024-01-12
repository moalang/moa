const parse = source => {
  if (source.trim().length === 0) {
    return []
  }
  const regexp = /([!+\-*/%<>:!=^|&]+|[()\[\]{}]|r?"[^]*?(?<!\\)"|r?'[^]*?(?<!\\)'|-?[0-9]+(?:\.[0-9]+)|[0-9A-Za-z_]+|(?:#[^\n]*|[ \n])+)/ // operator | parenthesis | string | number | id | comment and spaces
  const tokens = source.trim().split(regexp).filter(s => s.length)

  var pos = 0
  const read = () => (t => t.match(/^[ #]/) ? tokens[++pos] || '' : t)(tokens[pos] || '')
  const loop = (a, f) => pos < tokens.length ? (v => v ? loop(a.concat([v]), f) : a)(f(read())) :a
  const many = f => loop([], f)
  const until = s => many(t => (t.includes('\n') && ++pos, read() === s ? (++pos, null) : parse_exp()))
  const consume = () => (t => ++pos && t)(read() || fail('out of index', pos, tokens))
  const indent = t => t.includes('\n') ? t.split('\n').at(-1).length : fail('not break line', t)
  const call = a => a.length === 1 ? ['__call', ...a] : a
  const squash = a => a.length === 1 ? a[0] : a
  const pack = a => a.length === 1 ? a[0] : a.length > 1 ? ['__pack', ...a] : a
  const parse_unit = () => {
    const suffix = t => {
      const close = tokens[pos] || ''
      const next = read()
      return close === '('  ? ++pos && suffix(call([t, ...until(')')])) :
             close === '['  ? ++pos && suffix(call(['__index', t, ...until(']')])) :
             next  === ','  ? suffix([t, ...many(t => t === ',' && ++pos && consume())]) :
             next  === '.'  ? ++pos && suffix([next, t, consume()]) :
             next  === '=>' ? ++pos && ['fn', t, parse_block()] :
             t
    }
    const t = consume()
    return suffix(
      t === '!' ? [t, parse_unit()] :
      t === '-' ? [t, parse_unit()] :
      t === '[' ? call(['list', ...until(']')]) :
      t === '(' ? squash(until(')')) :
      t === ':' ? parse_block() :
      t)
  }
  const parse_exp = () => {
    const lhs = parse_unit()
    const is_op2 = s => typeof s === 'string' && s.match(/^:?[!+\-*/%<>!=^~|&]/) && s !== '!'
    const op2s = '< &&'.split(' ')
    const priority = op => op2s.findIndex(x => x === op)
    const sort = (op, lhs, rhs) =>
      Array.isArray(rhs) && is_op2(rhs[0]) && priority(op) < priority(rhs[0]) ? [rhs[0], [op, lhs, rhs[1]], rhs[2]] :
      [op, lhs, rhs]
    return is_op2(read()) ? (op => sort(op, lhs, parse_exp()))(consume()) : lhs
  }
  const is_stop = t => t.includes('\n') || t === ')' || t === ']' || t === ';'
  const parse_line = () => {
    const a = squash(many(t => !is_stop(t) && parse_exp()))
    const remain = []
    while (read() === ";" && ++pos) {
      remain.push(squash(many(t => !is_stop(t) && parse_exp())))
    }
    return remain.length ? pack([a, ...remain]) : a.length && a
  }
  const parse_lines = n => pack(many(t => (t.includes('\n') && indent(t) === n && ++pos, parse_line())))
  const parse_block = () => (t => t.includes('\n') ? parse_lines(indent(t)) : parse_line())(read())
  return parse_lines(0)
}
