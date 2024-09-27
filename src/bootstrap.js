'use strict'

function run(source, env) {
  return evaluate(parse(tokenize(source)), env || {})
}

function evaluate(root, env) {
  function rec(node) {
    return evaluate(node, env)
  }
  function recWith(node, args, types) {
    const params = types.map((t, i) => [t.text, args[i]])
    return evaluate(node, {...env, ...Object.fromEntries(params)})
  }
  function eq(a, b) {
    return typeof a === typeof b && Array.isArray(a) && Array.isArray(b) &&
      Array.isArray(a) ? a.length === b.length && a.every((x, i) => eq(x, b[i])) :
      typeof a === 'object' ? eq(Object.keys(a).sort(), Object.keys(b).sort()) && Object.keys(a).every(k => eq(a[k], b[k])) :
      a === b
  }
  function lt(a, b) {
    return typeof a === typeof b && Array.isArray(a) && Array.isArray(b) &&
      Array.isArray(a) ? a.every((x, i) => lt(x, b[i])) :
      typeof a === 'object' ? Object.keys(a).every(k => lt(a[k], b[k])) :
      a < b
  }
  function le(a, b) {
    return eq(a, b) || lt(a, b)
  }
  function gt(a, b) {
    return lt(b, a)
  }
  function ge(a, b) {
    return le(b, a)
  }
  const props = {
    object: o => o,
    string: s => ({
      size: s.length,
      has(ss) { return s.includes(ss) },
      int: parseInt(s),
      float: parseFloat(s),
    })
  }
  function prop(o, name) {
    const table = props[typeof o](o)
    return name.text in table ? table[name.text] : fail(`no field '${name.text}' of '${typeof o}'`, name)
  }
  function line(caller) {
    return Array.isArray(caller) ? line(caller[0]) : caller.line
  }
  const tester = {
    eq(a, b, caller) {
      return eq(a, b) || fail(`${line(caller)}: '${a}' != '${b}'`)
    }
  }
  if (Array.isArray(root)) {
    const head = root[0]
    const tail = root.slice(1)
    switch (head.text) {
      case '!': return !rec(tail[0])
      case '.': return prop(rec(tail[0]), tail[1])
      case '&&': return rec(tail[0]) && rec(tail[1])
      case '||': return rec(tail[0]) || rec(tail[1])
      case '==': return eq(rec(tail[0]), rec(tail[1]))
      case '!=': return !eq(rec(tail[0]), rec(tail[1]))
      case '<': return lt(rec(tail[0]), rec(tail[1]))
      case '<=': return le(rec(tail[0]), rec(tail[1]))
      case '>': return gt(rec(tail[0]), rec(tail[1]))
      case '>=': return ge(rec(tail[0]), rec(tail[1]))
      case '+': return rec(tail[0]) + rec(tail[1])
      case '-': return rec(tail[0]) - rec(tail[1])
      case '*': return rec(tail[0]) * rec(tail[1])
      case '/': return rec(tail[0]) / rec(tail[1])
      case '%': return rec(tail[0]) % rec(tail[1])
      case '__stmt': return tail.map(rec).at(-1)
      case 'def': return env[tail[0].text] = (...args) => recWith(tail.at(-1), args, tail.slice(1, -1))
      case 'test': return recWith(tail.at(-1), [tester], tail.slice(0, -1))
      default:
        const f = Array.isArray(head) ? rec(head) : head.text in env ? env[head.text] : fail(`no id '${head.text}'`, head)
        return f(...tail.map(rec), head)
    }
  } else {
    const t = root.text
    return /^[0-9]+\./.test(t) ? parseFloat(t) :
      /^[0-9]/.test(t) ? parseInt(t) :
      t[0] === '"' ? t.slice(1, -1) :
      t === 'true' ? true :
      t === 'false' ? false :
      t in env ? env[t] :
      fail(`no id '${t}'`, root)
  }
}

function parse(tokens) {
  const len = tokens.length
  let index = 0
  function bottom() {
    const node = consume()
    switch (node.text) {
      case '!':
        return [node, exp()]
      case '(':
        const ret = exp()
        consume().text !== ')' && fail('( is not close', node)
        return ret
      case 'def':
      case 'test':
        const a = [node].concat(line(node.line, t => t.text === ':'))
        if (tokens[index].line === node.line) {
          return a.concat([exp()])
        } else {
          return a.concat([block(tokens[index])])
        }
      default:
        return node
    }
  }
  function exp() {
    return consumeWith(bottom())
  }
  function line(line, f) {
    const a = []
    while (index < len && tokens[index].line === line) {
      const t = consume()
      if (f(t)) {
        break
      } else {
        a.push(t)
      }
    }
    return a
  }
  function block(base) {
    const line = base.line
    const indent = base.indent
    const a = [{text: '__stmt', line, indent}]
    while (index < len && tokens[index].indent === indent) {
      a.push(exp())
    }
    return a
  }
  function until(start, mark) {
    const a = []
    while (index < len) {
      const token = exp()
      if (token.text === mark) {
        return a
      } else {
        a.push(token)
      }
    }
    fail(`no close '${start.text}'`, start)
  }
  function consumeWith(node) {
    if (index < len) {
      switch (tokens[index].text) {
        case '&&':
        case '||':
        case '==':
        case '!=':
        case '>':
        case '<':
        case '>=':
        case '<=':
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
          const op2 = consume()
          return [op2, node, exp()]
        case '.':
          const dot = consume()
          const ref = [dot, node, consume()]
          return consumeWith(ref)
        case '(':
          if (index > 0 && tokens[index-1].pos + tokens[index-1].text.length === tokens[index].pos) {
            return consumeWith([node].concat(until(consume(), ')')))
          }
        default:
          return node
      }
    } else {
      return node
    }
  }
  function consume() {
    return tokens[index++] || fail('EOT', tokens.at(-1))
  }
  const nodes = []
  while (index < len) {
    nodes.push(exp())
  }
  if (index < tokens.length) {
    fail('failed to parse', `${ index } < ${ tokens.length }`)
  }
  return nodes.length === 1 ? nodes[0] : [{text: '__stmt', line: 1, indent: 0}].concat(nodes)
}

function tokenize(source) {
  const len = source.length
  const tokens = []
  let line = 1
  let indent = 0
  let token = null
  function flush() {
    if (token) {
      tokens.push(token)
      token = null
    }
  }
  for (let i=0; i<len; ++i) {
    const c = source[i]
    switch (c) {
      case '\n':
        line += 1
        indent = source.slice(i+1).match(/ */)[0].length
        flush()
        break
      case ' ':
        flush()
        break
      case '&':
        flush()
        if (source[i+1] === '&') {
          tokens.push({text: '&&', pos: i, line, indent})
          i += 1
        }
        break
      case '<':
      case '>':
      case '!':
        if (source[i+1] === '=') {
          tokens.push({text: c + '=', pos: i, line, indent})
          i += 1
          break
        }
      case ':':
      case '(':
      case ')':
        flush()
        tokens.push({text: c, pos: i, line, indent});
        break
      default:
        if (c === '.' && /[a-zA-Z_]/.test(source[i+1])) {
          flush()
          tokens.push({text: c, pos: i, line, indent});
          break
        }
        if (!token) {
          token = {text: c, pos: i, line, indent}
        } else {
          token.text += c
        }
    }
  }
  token && tokens.push(token)
  return tokens
}

function fail(message, info) {
  const e = new Error(message)
  e.info = info
  throw e
}

function log(...a) {
  console.dir(a, {depth: null})
  return a[0]
}

module.exports = { run }
