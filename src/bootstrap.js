'use strict'

// TODO
// - syntax sugar: do (a) (b) to do:\n  a\n  b

const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
const str = JSON.stringify
const eq = (a,b) => str(a) === str(b)
const isArray = o => typeof o === 'object' && o.constructor === Array
const op2 = '+ - * / += -= *= /= == != && ||'.split(' ')
const isOp2 = t => op2.includes(t)
const dict = a => {
  const length = Math.floor(a.length / 2)
  const d = {}
  Array.from({length}, (_, i) => d[a[i*2]] = a[i*2+1])
  return d
}

const tokenize = src => {
  let string = ''
  const stack = []
  const push = () => {
    if (string !== '') {
      stack.push(string)
      string = ''
    }
  }
  for (const c of src) {
    if (c === '(' || c === ')') {
      push()
      string = c
      push()
    } else if (c === ' ' || c === '\n') {
      push()
    } else {
      string += c
    }
  }
  push()
  return stack
}
const parse = tokens => {
  const top = []
  let stack = [top]
  for (const token of tokens) {
    if (token === '(') {
      const a = []
      stack[stack.length - 1].push(a)
      stack.push(a)
    } else if (token === ')') {
      stack = stack.slice(0, -1)
    } else if (isOp2(token)) {
      const a = stack[stack.length - 1]
      if (a.length) {
        const lhs = a[a.length - 1]
        a[a.length - 1] = token
        a.push(lhs)
      } else {
        stack[stack.length - 1].push(token)
      }
    } else {
      stack[stack.length - 1].push(token)
    }
  }
  return top
}
const generate = nodes => {
  const value = v => typeof v === 'string' && v.match(/^[0-9](\.[0-9]+)?$/) ? parseInt(v) : v
  const addReturn = a => (a[a.length-1] = `return ${a[a.length-1]}`,a)
  const gen = node => {
    if (isArray(node) && node.length === 1) {
      return value(node[0])
    }
    switch (node[0]) {
      case 'def': return `const ${node[1]} = (${node.slice(2, -1).join(',')}) => ${gen(node[node.length - 1])}`
      case 'struct':
        const names = node[node.length - 1].map(field => field[0]).join(',')
        return `const ${node[1]} = (${names}) => ({${names}})`
      case 'array': return `[${node.slice(1).map(gen).join(',')}]`
      case 'dict': return `(${str(dict(node.slice(1).map(gen)))})`
      case 'do': return `{${addReturn(node.slice(1).map(gen)).join(';')}}`
      case 'var': return `let ${node[1]} = ${gen(node.slice(2))}`
      case 'let': return `const ${node[1]} = ${gen(node.slice(2))}`
      case 'if': return `(${gen(node[1])} ? ${gen(node[2])} : ${gen(node[3])})`
      case '.':
        return `(${gen(node[1])}).${node[2]}`
      default:
        if (isOp2(node[0])) {
          return gen(node[1]) + node[0] + gen(node[2])
        } else if (isArray(node)) {
          if (node.length === 1) {
              return gen(node[0])
          } else {
              return node[0] + `(${node.slice(1).map(gen)})`
          }
        } else {
          return value(node)
        }
    }
  }
  return nodes.map(gen).join('\n')
}
const test = () => {
  const t = (expect, exp, ...defs) => {
    const src = defs.concat([`(def main (${exp}))`]).join('\n')
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    const js = generate(nodes)
    let actual
    try {
      actual  = Function(js + '\nreturn main()')()
    } catch(e) {
      actual = e.message
      puts(e.stack)
    }
    if (eq(expect, actual)) {
      process.stdout.write('.')
    } else {
      puts('src:', js)
      puts('expect:', expect)
      puts('actual:', actual)
      dump(nodes)
      process.exit(1)
    }
  }
  // primitives
  t(1, '1')
  t(3, '1 + 2')
  t([1, 2], 'array 1 2')
  t({1: 2, 3: 4}, 'dict 1 2 3 4')

  // function
  t(3, 'add 1 2', '(def add a b (a + b))')
  t(6, 'calc 2 3', '(def calc a b (do (def mul a b (a * b)) (mul a b)))')

  // struct
  t({x:1, y:2}, 'vector2 1 2', '(struct vector2 ((x int) (y int)))')
  t(2, '. (vector2 1 2) y', '(struct vector2 ((x int) (y int)))')

  // constant
  t(1, 'do (let a 1) (a)')

  // variable
  t(3, 'do (var a 1) (a += 2) (a)')

  // branch
  t(1, 'if true 1 2')
  t(2, 'if false 1 2')
  t(2, 'if (true && (1 == 2)) 1 2')

//  // option
//  t(3, 'then(1 v => (v + 2))')
//  f('failure', 'error("failure")')
//  f('failure', 'then(error("failure") v => v)')
//  t('failure', 'catch(error("failure") e => e.message)')
//
//  // match
//  t(1, 'match(a a 1 b 2)', 't|\n  a\n  b')
//  t(2, 'match(b a 1 b 2)', 't|\n  a\n  b')
//  t(3, 'match(b(2) a 1 b inc)', 't|\n  a\n  b:\n    num int', 'inc o = o.num + 1')
//  t(1, 'match(e1 e1 1 e2 2 _ 3)', 'e1 = error(1)', 'e2 = error(2)')
//
//  // monadic statement
//  t(1, '\n  1')
//  t(2, '\n  1\n  2')
//  t(5, '\n  a <- f(1)\n  b <- f(a)\n  a + b', 'f v = v + 1')
//  f("failure", '\n  error("failure")\n  2')
//  f('error.if', '\n  e.if(true)\n  1', 'e = error("error.if")')
//  t(1, '\n  e.unless(true)\n  1', 'e = error("error.unless")')
//
//  // modify variable
//  t(3, '\n  a <- 1\n  a += 2\n  a')
//  t(1, '\n  a <- 1\n  a0(a)\n  a', 'a0 a = a := 0')
//  t(3, '\n  a <- 1\n  inc =\n    a += 1\n  inc\n  inc\n  a')
//  t(6, '\n  a <- 1\n  add n =\n    a += n\n  add(2)\n  add(3)\n  a')



  puts('ok')
}
test()
