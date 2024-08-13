'use strict'
// Compile AST to code

const log = o => { console.dir(o, {depth: null}); return o }
const str = o => JSON.stringify(o, null, '  ')
const fail = (m, ...a) => { const e = new Error(m); a && (e.detail = a); throw e }

const compile = root => {
  const prefix = '___'
  const op1 = new Set('! ~'.split(' '))
  const op2 = new Set('|| && + - * ** / %  & | ^ << >> != == < <= >= > = '.split(' '))
  const embedded = new Set('bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert throw'.split(' '))
  const global = new Set([
    'float inf',
    'float nan',
  ])
  const iifjs = a =>
    a.length === 0 ? fail(`invalid the number of arguments of iif`) :
    a.length === 1 ? a[0] :
    `${a[0]} ? ${a[1]} : ${iifjs(a.slice(2))}`
  const lines = x => !Array.isArray(x) ? [[x]] :
    x[0]?.code === ':' ? lines(x[1]) :
    x[0]?.code === '__block' ? x.slice(1).map(x => Array.isArray(x) ? x : [x]) :
    [x]
  const tojs = (node) => {
    if (Array.isArray(node)) {
      const [head,...tail] = node
      if (head.code === '.') {
        if (global.has(tail.map(t => t.code).join(' '))) {
          return `${prefix}${tail[0].code}_${tail[1].code}`
        } else {
          const type = tail[0].type
          const target = tojs(tail[0])
          const method = tail[1].code
          return `${prefix}${type}_${method}(${target})`
        }
      } else if (head.code === ':') {
        return tojs(tail[0])
      } else if (head.code === 'iif') {
        return '(' + iifjs(tail.map(tojs)) + ')'
      } else if (head.code === 'if') {
        const cond = tojs(tail.length === 2 ? tail[0] : tail.slice(0, -1))
        const body = tojs(tail.at(-1))
        return `if (${cond}) {${body}}`
      } else if (head.code === 'else') {
        if (tail[0].code === 'if') {
          return `else ${tojs(tail)}`
        } else {
          const body = tojs(tail.at(-1))
          return `else {${body}}`
        }
      } else if (head.code === 'switch') {
        const cond = tojs(tail.length === 2 ? tail[0] : tail.slice(0, -1))
        const name = tail[0].type
        const switchjs = a =>
          a.length === 2 ? `__s.__tag === "${name}.${a[0].code}" ? ${tojs(a[1])} :` :
          a.length === 3 ? `__s.__tag === "${name}.${a[0].code}" ? (${a[1].code} => ${tojs(a[2])})(__s.__val) :` :
          fail(`Unknown switch ${str(a)}`)
        const x = tail.at(-1)[1]
        const body = (Array.isArray(x[0]) ? x : [x]).map(switchjs).join('\n')
        return `(__s => ${body} moa.throw("switch", __s))(${cond})`
      } else if (head.code === 'for') {
        const a = tail[0].code
        const b = tail[1]?.code
        const c = tail[2]?.code
        const d = tail[3]?.code
        const body = tojs(tail.at(-1))
        return tail.length === 3 ? `for (let ${a}=0; ${a}<${b}; ++${a}) {${body}}` :
          tail.length === 4 ? `for (let ${a}=${b}; ${a}<${c}; ++${a}) {${body}}` :
          tail.length === 5 ? `for (let ${a}=${b}; ${a}<${c}; ${a}+=${d}) {${body}}` :
          fail(`Unknown for ${str(tail)}`)
      } else if (head.code === 'while') {
        const cond = tojs(tail.length === 2 ? tail[0] : tail.slice(0, -1))
        const body = tojs(tail.at(-1))
        return `while (${cond}) {${body}}`
      } else if (head.code === 'let') {
        const value = tojs(tail.length === 2 ? tail[1] : tail.slice(1, -1))
        return `const ${tail[0].code} = ${value}`
      } else if (head.code === 'var') {
        const value = tojs(tail.length === 2 ? tail[1] : tail.slice(1, -1))
        return `let ${tail[0].code} = ${value}`
      } else if (head.code === 'def') {
        const name = tail[0].code
        const args = tail.slice(1, -1).map(x => x.code).join(', ')
        const last = tail.at(-1)[0]?.code === ':' ? tail.at(-1)[1] : tail.at(-1)
        const steps = (last[0]?.code === '__block' ? last.slice(1) : [last]).map(tojs)
        const body = [...steps.slice(0, -1), 'return ' + steps.at(-1)].join('\n')
        return `function ${name}(${args}) {${body}}`
      } else if (head.code === 'class') {
        const name = tail[0].code
        const fields = lines(tail.at(-1)).map(x => x[0].code).join(', ')
        return `function ${name}(${fields}) { return {${fields}} }`
      } else if (head.code === 'enum') {
        const name = tail[0].code
        const enumjs = a => a.length === 1 ? `const ${a[0].code} = {__tag: "${name}.${a[0].code}"}` :
          a.length === 2 && Array.isArray(a[1]) ?
          (a1 => `function ${a[0].code}(${a1.map(x => x[0].code).join(', ')}) { return {__tag: "${name}.${a[0].code}", __val: {${a1.map(x => x[0].code).join(', ')}}} }`)(lines(a[1])) :
          a.length === 2 ? `function ${a[0].code}(__val) { return {__tag: "${name}.${a[0].code}", __val} }` :
          fail(`Unknown enum ${str(name)} with ${str(a)}`)
        return lines(tail.at(-1)).map(enumjs).join('\n')
      } else if (head.code === 'catch') {
        const target = tojs(tail[0])
        const handle = tojs(tail[1])
        return `(() => { try { return ${target} } catch (e) { return ${handle}(${prefix}error(e)) } })()`
      } else if (head.code === '__block') {
        return tail.map(tojs).join('\n')
      } else if (head.code === 'dec' || head.code === 'interface' || head.code === 'extern') {
        return ''
      } else if (head.code === '-' && tail.length === 1) {
        return `(-${tojs(tail[0])})`
      } else if (op1.has(head.code)) {
        return `(${head.code}${tojs(tail[0])})`
      } else if (op2.has(head.code)) {
        return `(${tojs(tail[0])} ${head.code} ${tojs(tail[1])})`
      } else if (tail.length === 1 && tail[0].length === 0) {
        return tojs(head) + '()'
      } else {
        return `${tojs(head)}(${tail.map(tojs).join(', ')})`
      }
    } else {
      return embedded.has(node.code) ?  `${prefix}${node.code}` : node.code
    }
  }
  try {
    return tojs(root)
  } catch (e) {
    e.detail = root
    throw e
  }
}

module.exports = { compile }
