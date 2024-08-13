#!node
'use strict'

/*
top: exp+ ([\n;] exp+)*
exp:
| op1? atom (op2 exp)?
| id ("," id+ )* "=>" exp
atom: bottom (prop | call | copy | slice)*
prop: "." (id | [0-9]+)                   # property access
call: "(" exp* ")"                        # call function
index: "[" exp* "]"                       # index access or generic
copy: "{" top "}"                         # copy with new value
slice: "[" exp? ":" exp? "]"              # slice
bottom:
| call | index | copy | slice
| "-"? [0-9]+ ("." [0-9]+)? ("e" [0-9]+)? # -1.2
| "-"? "0x" [0-9a-fA-F_]+                 # 0xff -> 255
| '"' [^"]* '"'                           # "string"
| '"""' [^"]* '"""'                       # """a="b"""" -> "a=\"b\""
| id
op1: [!-~] | "..."
op2: [+-/*%<>|&^=!]+
id: [A-Za-z_][A-Za-z0-9_]*
comment: "//" [^\n]*
*/

class TypeError extends Error {}
const log = o => { console.dir(o, {depth: null}); return o }
const str = o => JSON.stringify(o, null, '  ')
const fail = (m, ...a) => { const e = new Error(m); a && (e.detail = a); throw e }
const failUnify = (m, ...a) => { const e = new TypeError(m); a && (e.detail = a); throw e }
const runtimeJs = (function() {'use strict'
const ___string = o => typeof o === 'string' ? o :
  o instanceof Array ? `(list ${o.map(___string).join(' ')})` :
  o instanceof Map ? `(dict ${[...o].map(___string).join(' ')})` :
  o instanceof Set ? `(dict ${[...o].map(___string).join(' ')})` :
  o.toString()
const ___throw = (m, d) => { const e = new Error(m); e.detail = d; throw e }
const ___dict_set = (m, k, v) => (m.set(k, v), v)
const ___assert = (a, b) => ___string(a) === ___string(b) || ___throw(`Assert failure: \`${___string(a)}\` is not \`${___string(b)}\``, a, b)
const ___tuple = (...a) => a
const ___list = (...a) => a
const ___dict = (...a) => new Map([...Array(a.length/2)].map((_,i) => [a[i*2], a[i*2+1]]))
const ___log = (...a) => (console.log(...a.map(___string)), a.at(-1))
}).toString().slice(12, -1) + '\n'

function main(command, args) {
  const { readFileSync } = require('fs')
  if (command === 'to' && args[0] === 'js') {
    return { out: runtimeJs + toJs(args.slice(1).join(' ') || readFileSync('/dev/stdin', 'utf-8')) }
  } else {
    return { out: `Usage:
      moa                       # launch interactive shell
      moa env [+/-] [<version>] # list versions; use, install or remove a version
      moa ide [<port>]          # launch web IDE
      moa to [<language>]       # compile to a programming language` }
  }
}

function tokenize(source) {
  const regexp = /([!~+\-*/%<>:!=^|&]+|[()\[\]{}]|""".*?"""|"[^]*?(?<!\\)"|-?[0-9]+[0-9_]*(?:\.[0-9_]+)|[0-9A-Za-z_]+|(?:#[^\n]*|[ \n])+)/
  let offset = 0
  const tokens = source.trim().split(regexp).flatMap(code => code.length ? [{code, offset: offset+=code.length}] : [])
  return tokens
}

function parse(tokens) {
  return tokens[0]
}

function infer(root) {
  return root
}

function compileToJs(root) {
  return root.code
}

function toJs(source) {
  return compileToJs(infer(parse(tokenize(source))))
}

module.exports = { main, runtimeJs, toJs, TypeError }

if (require.main === module) {
  console.log(main(process.argv[2], process.argv.slice(3)).out || '')
}
