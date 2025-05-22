const newtoken = (s, filename, lineno=0, column=0) => (m => m ? ({code: m[1], mark: m[2], filename, lineno, column}) : ({code: s, filename, lineno, column}))(s.match(/^([A-Za-z_].*)([?*])$/))

const parseWithoutSugar = program => {
  function* tokenize() {
    for (const code of program.split(/([()]|[^\n ()]+)/)) {
      if (code.trim()) {
        yield newtoken(code)
      }
    }
  }
  const compose = ([t, ...ts], acc=[]) =>
    !t ? acc :
    t.code === ")" ? [acc, compose([ts])] :
    t.code === "(" ? (([x, rest]) => compose(rest, acc.concat([x])))(compose(ts)) :
    compose(ts, acc.concat(t))
  return compose([...tokenize()])
}

const parse = (program, filename="") => {
  function* tokenize() {
    let pos = 0
    let lineno = 1
    let column = 0
    let indent = 0
    for (const code of program.split(/([A-Za-z_][A-Za-z0-9_]*[?*]?|-?0x[A-Fa-f0-9]+|-?[0-9]+(?:(?:\.[0-9]+)|(?:e[0-9]+))?|""".*?"""|"[^"]*?"|```.*?```|`[^`]*?`|[ \r\n\t]+|[()\[\]{};]|#[^\n]*)/)) {
      if (/^[ \r\n\t#;]/.test(code) || code === "") {
        lineno += code.split(/\n|;/).length - 1
        if (code.includes("\n")) {
          indent = code.split(/\n/).at(-1).length
        }
      } else {
        const op = /^[+\-*/%|&<>=!]+$/.test(code) && code !== "=>"
        const lc = pos > 0 ? program[pos - 1] : ''
        const rc = program[pos + code.length]
        const op1 = op && /[A-Za-z0-9_\]]/.test(rc)
        const op2 = op && !op1
        const call = code === "(" && /[A-Za-z0-9_\]]/.test(lc)
        const index = code === "[" && /[A-Za-z0-9_\]]/.test(lc)
        yield {code, lineno, op, op1, op2, call, index, indent, filename, lineno, column}
      }
      pos += code.length
      column = code.includes("\n") ? code.split(/\n/).at(-1).length : column + code.length
    }
  }
  const compose = tokens => {
    let pos = 0
    const until = (f, g=bottom) => [...function*() { while (pos < tokens.length && f(tokens[pos])) { yield g() } }()]
    const faketoken = (s, t) => newtoken(s, filename, t.lineno, t.column)
    const isexp = x => !Array.isArray(x) || x[0].code !== "do"
    const untilby = by => (t => (pos++, t))(until(t => t.code !== by))
    const bottom = () => {
      const totoken = o => Array.isArray(o) ? totoken(o[0]) : o
      const comma = a => pos >= tokens.length ? a :
        tokens[pos].code === "," ? (pos++, comma(a.concat([tokens[pos++]]))) : a
      const block = t => t.lineno === tokens[pos].lineno ? line() :
        squash(until(u => u.indent > t.indent, line), a => [faketoken("do", t), ...a])
      const link = t =>
        pos >= tokens.length ? t :
        tokens[pos].code === "."  ? link([tokens[pos++], t, tokens[pos++]]) :
        tokens[pos].code === ","  ? link(comma([t])) :
        tokens[pos].code === "=>" ? (pos++, link([faketoken("fn", tokens[pos-1]), ...(Array.isArray(t) ? t : [t]), block(tokens[pos-1])])) :
        tokens[pos].call          ? (pos++, link([t].concat(untilby(")")))) :
        tokens[pos].index         ? (pos++, link([[faketoken(".", t), t, faketoken("at", t)]].concat([squash(variadic(untilby("]")))]))) :
        tokens[pos].op2           ? link([tokens[pos++], t, bottom()]) :
        t
      const t = tokens[pos++]
      return t.code === ":" ? block(t) :
             t.code === "(" ? variadic(untilby(")")) :
             t.op1          ? link([t, bottom()]) :
             link(t)
    }
    const line = () => (t => squash(variadic(until(u => t.lineno === u.lineno))))(tokens[pos])
    const top = () => until(_ => true, line)
    const squash = (a, f=x=>x) => a.length === 1 ? a[0] : f(a)
    const variadic = ([head, ...a]) => {
      const fail = x => { throw new Error(`never reach bug '${JSON.stringify(x)}'`) }
      const vop = (op, a) => [op, a[0], a.length === 2 ? a[1] : vop(op, a.slice(1))]
      const viif = (head, a) => [head, a[0], a[1], a.length === 3 ? a[2] : viif(head, a.slice(2))]
      const vif = (head, a) =>
        a.length >= 5 ? [head, a[0], a[1], vif(head, a.slice(2))] :
        a.length >= 4 ? [head, a[0], [head, ...a.slice(1)]] :
        [head, ...a]
      const t_ = faketoken("_", head)
      const t0 = faketoken("0", head)
      const t1 = faketoken("1", head)
      return head.op && a.length >= 3 ? vop(head, a) :
        head.code === "iif" ? viif(head, a) :
        head.code === "if"  ? vif(head, a) :
        head.code === "for" ? (
          a.length === 2 ? [head, t_,   t0,   a[0], t1, a[1]] :
          a.length === 3 ? [head, a[0], t0,   a[1], t1, a[2]] :
          a.length === 4 ? [head, a[0], a[1], a[2], t1, a[3]] :
          a.length === 5 ? [head, ...a] :
          fail(a)
        ) :
        head.code === "each" ? (
          a.length === 3 ? [head, t_, ...a] :
          a.length === 4 ? [head, ...a] :
          fail(a)
        ) :
        [head, ...a]
    }
    return top()
  }
  const normalize = o => Array.isArray(o) ? o.map(normalize) : newtoken(o.code, o.filename, o.lineno, o.column)
  return normalize(compose([...tokenize()]))
}

export {parse, newtoken, parseWithoutSugar}
