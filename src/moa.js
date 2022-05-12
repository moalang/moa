'use strict'

const fs = require('fs')
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const trace = (...a) => puts('TRACE', ...a)
const dump = o => console.dir(o, {depth: null})
const reserves = 'use fn struct enum let var if else elif for while continue break return p pp'.split(' ')
const fail = m => { throw Error(m) }

Object.defineProperty(String.prototype, '_size', { get() { return this.length } });
String.prototype._at = function (n) { return n >= this.length || n < -this.length ? fail('Out of index') : n >= 0 ? this[n] : this[this.length + n] }
String.prototype._in = function (s) { return this.includes(s) }
String.prototype._sub = function (a, b) { return this.replaceAll(a, b) }
String.prototype._split = function (s) { return this.split(s) }
String.prototype._rsub = function (a, b) { return this.replaceAll(new RegExp(a, 'g'), b) }
String.prototype._rsplit = function (r) { return this.split(new RegExp(r, 'g')) }
Object.defineProperty(Array.prototype, '_size', { get() { return this.length } });
Array.prototype._at =  function (n) { return n >= this.length || n < -this.length ? fail('Out of index') : n >= 0 ? this[n] : this[this.length + n] }
Array.prototype._append = function (a) { return this.concat(a) }
Array.prototype._contains = function (s) { return this.includes(s) }
Array.prototype._keep = function (f) { return this.filter(f) }
Object.defineProperty(Boolean.prototype, '_flip', { get() { return this == false } })

const isOp2 = s => s && s.match && s.match(/^[+\-*%/=><|&.]+$/)
const compile = source => {
  const simplify = ts => {
    const tokens = []
    let level = 0
    for (const t of ts) {
      if (t === ' ' || t.match(/^ +$/)) {
        continue
      } else if (t.match(/^ *$/)) {
        continue
      } else if (t.includes('\n')) {
        if (level === 0) {
          tokens.push(';')
        }
        continue
      }
      if ('[('.includes(t)) {
        ++level
      } else if (')]'.includes(t)) {
        --level
      } else if (t === '}' && tokens[tokens.length - 1] !== ';') {
        tokens.push(';')
      }
      tokens.push(t)
    }
    return tokens
  }
  const tokens = simplify(source.split(/([A-Za-z0-9_]+\(|"[^"]*"|[^\[\](){} \n;\.]+|.)/g))
  const nodes = parse(tokens)
  const js = generate(nodes)
  return {tokens, nodes, js}
}

const parse = tokens => {
  let pos = 0
  const many = (f, option) => {
    option = option || {}
    const a = []
    while (pos < tokens.length) {
      if (option.stop && option.stop(tokens[pos])) {
        ++pos
        break
      }
      a.push(f(tokens[pos]))
    }
    return a
  }
  const consume = t => reserves.includes(t.toString()) ? line() : exp()
  const line = () => many(exp, {stop: t => t === ';'})
  const exp = () => ((lhs, t) => isOp2(t) ? ++pos && [t, lhs, exp()] : lhs)(atom(), tokens[pos])
  const atom = () => {
    if (pos >= tokens.length) {
      return null
    }
    const t = tokens[pos++]
    if (t.match(/^[0-9](\.[0-9]+)?$/) || t.match(/^[A-Za-z0-9_]+$/) || t.startsWith('"')) {
      return t
    } else if (t.match(/^[A-Za-z0-9_]+\($/)) {
      const args =  many(exp, {stop: u => u === ')'})
      return args.length ? [t.slice(0, -1),  ...args] : t + ')'
    } else if ('}]);'.includes(t.toString())) {
      return t
    } else if (t === '(') {
      return many(exp, {stop: u => u === ')'})
    } else if (t === '[') {
      return ['__array', ...many(exp, {stop: u => u === ']'})]
    } else if (t === '{') {
      return ['__do', ...many(line, {stop: u => u === '}'})]
    } else {
      throw Error(`Unexpected token "${t}"`)
    }
  }
  return many(consume)
}

const generate = nodes => {
  const gen = o => Array.isArray(o) ?(o.length === 1 ? gen(o[0]) : apply(o)) : o
  const isCond = args => ['if', 'unless'].includes(args[args.length - 2])
  const cond = (head, args) => args.length === 0 ? head :
    args[0] === 'if' ? `if (${gen(args[1])}) ${head}` :
    args[0] === 'unless' ? `if (!${gen(args[1])}) ${head}` :
    fail(`Unknown condition ${args}`)
  const apply = ([head, ...args]) => {
    if (isOp2(head)) {
      if (head === '.') {
        return '(' + gen(args[0]) + '._' + args[1] + ')'
      } else {
        return '(' + gen(args[0]) + head + gen(args[1]) + ')'
      }
    } else if (head === '__array') {
      return '[' + args.map(gen).join(', ') + ']'
    } else if (head === '__do') {
      return '{' + args.map(gen).join('\n') + '}'
    } else if (head === 'let') {
      return `const ${args[0]} = ${gen(args.slice(1))}`
    } else if (head === 'var') {
      return `var ${args[0]} = ${args.length >= 2 ? gen(args.slice(1)) : 'undefined'}`
    } else if (head === 'fn') {
      return `const ${args[0]} = (${args.slice(1, -1)}) => ${gen(args[args.length - 1])}`
    } else if (head === 'struct') {
      const names = args[args.length - 1].slice(1).map(a => '_' + a[0])
      return `const ${args[0]} = (${names}) => ({${names}})`
    } else if (head === 'if') {
      return `if (${gen(args[0])}) ${gen(args[1])} ${args.length >= 3 ? gen(args.slice(2)) : ''}`
    } else if (head === 'elif') {
      return `else if (${gen(args[0])}) ${gen(args[1])} ${args.length >= 3 ? gen(args.slice(2)) : ''}`
    } else if (head === 'else') {
      return `else ${gen(args)}`
    } else if (head === 'for') {
      if (args.length == 3) {
        return `for (let ${args[0]} of ${gen(args[1])}) { ${gen(args[2])} }`
      } else {
        throw Error(`Unknown for syntax ${args}`)
      }
    } else if (head === 'while') {
      return `while (${gen(args[0])}) { ${gen(args[1])} }`
    } else if (head === 'continue' || head === 'break') {
      return cond(head, args)
    } else if (head === 'return') {
      if (args.length === 0) {
        return 'return'
      } else if (args.length === 1) {
        return `return ${gen(args[0])}`
      } else if (args.length === 2) {
        return cond('return', args)
      } else if (args.length === 3) {
        return cond(`return ${args[0]}`, args.slice(1))
      } else {
        throw Error(`Unknown return syntax ${args}`)
      }
    } else if (head === 'p' || head === 'pp') {
      if (isCond(args)) {
        return cond(`__${head}(${args.slice(0, -2).map(gen)})`, args.slice(-2)) + '; ""'
      } else {
        return `__${head}(${args.map(gen)}); ""`
      }
    } else {
      return `${head}(${args.map(gen)})`
    }
  }
  return nodes.map(gen).join(';\n')
}
const test = () => {
  const check = (expect, source) => {
    const {js, nodes, tokens} = compile(source)
    let actual
    try {
      const __stdout = []
      const __p = (...a) => __stdout.push(a.map(str).join(' '))
      const __pp = (...a) => __stdout.push(a.map(x => JSON.stringify(x, null, 2)).join(' '))
      actual = eval(js)
      if (__stdout.length) {
        actual += __stdout.join('\n')
      }
    } catch(e) {
      actual = e.stack
    }
    if (str(actual) === str(expect)) {
      put('.')
    } else {
      puts('FAILURE')
      puts('source:', source)
      puts('js    :', js)
      puts('nodes :', nodes)
      puts('tokens:', tokens)
      puts('expect:', expect)
      puts('actual:', actual)
      process.exit(1)
    }
  }

  // define:
  // | "let" id exp
  check(1, 'let a 1; a')
  // | "var" id exp
  check(3, 'var a 1; a += 2; a')
  // | "fn" id+ exp
  check(3, 'fn add a b a + b; add(1 2)')
  check(1, 'fn f 1; f()')
  // | "struct" id+ "{" (define lf)* } "}"
  check(11, 'struct item { name string; price int }; item("jar" 11).price')

  // exp: node (op2 exp)*
  check(3, '1 + 2')
  check(7, '1 + 2 * 3')
  check(9, '(1 + 2) * 3')

  // node: bottom ("." id ("(" exp+ ")"))*
  check(2, '[1 2].size')

  // bottom:
  // | "(" exp ")"                    # priority   : 1 * (2 + 3)
  check(1, '(1)')
  // | "[" exp* "]"                   # array      : [] [1 2]
  check([1], '[1]')
  check([1, 2], '[1 2]')
  // | '"' [^"]* '"'                  # string     : "hi"
  check('hi', '"hi"')
  // | id ("," id)* "=>" exp          # lambda
  // | [0-9]+ ("." [0-9]+)?
  check(1, '1')

  // statement:
  // | "if" exp exp ("elif" exp exp)* ("else" exp)?
  check(1, 'var a; if true { a = 1 } elif true { a = 2 } else { a = 3 }; a')
  check(2, 'var a; if false { a = 1 } elif true { a = 2 } else { a = 3 }; a')
  check(3, 'var a; if false { a = 1 } elif false { a = 2 } else { a = 3 }; a')
  // | "for" id exp exp
  check(3, 'var a 0; for i [1 2] { a += i }; a')
  // | "while" exp exp
  check(3, 'var a 0; while a < 3 { a += 1 }; a')
  // | "continue" cond?
  check(0, 'var a 0; for i [1 2 3] { continue; a += i }; a')
  check(4, 'var a 0; for i [1 2 3] { continue if i == 2; a += i }; a')
  check(4, 'var a 2; for i [1 2 3] { continue unless i == 2; a += i }; a')
  // | "break" cond?
  check(0, 'var a 0; for i [1 2 3] { break; a += i }; a')
  check(1, 'var a 0; for i [1 2 3] { break if i == 2; a += i }; a')
  check(0, 'var a 0; for i [1 2 3] { break unless i == 2; a += i }; a')
  // | "return" exp? cond?
  check(1, 'fn f { return 1; return 2 }; f()')
  check(2, 'fn f { return 1 if false; return 2 }; f()')
  check(1, 'fn f { return 1 unless false; return 2 }; f()')
  // | "p" exp+ cond?                 # print one line while developing
  check('1 [2,3]\n4', 'p 1 [2 3]; p 4')
  check('1', 'p 1 if true')
  check('', 'p 1 unless true')
  // | "pp" exp+ cond?                # print multiple lines while developing
  check('1 [\n  2,\n  3\n]\n4', 'pp 1 [2 3]; pp 4')
  check('1', 'pp 1 if true')
  check('', 'pp 1 unless true')

  // block: "{" ((define | statement | exp) lf)* "}"
  check(1, '{1}')
  check(2, '{1;2}')
  check(2, '{1\n2}')
  check(1, '{let a 1\na}')

  // new lines
  check(1, '(\n(\n1\n)\n)')
  check(9, '(\n1\n+\n2\n) * 3')
  check([1, 2], '[\n1\n2\n]')

  // lines
  puts('ok')
}
const main = () => process.argv[2] ? console.log(compile(process.argv[2]).js) : test()

main()
