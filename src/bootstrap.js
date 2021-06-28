// Thanks
// https://github.com/reki2000/hyndley-milner-kotlin/blob/master/HindleyMilner.kt
// http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-10w/testcases.html

const write = (...a) => a.map(o => process.stdout.write(o.toString()))
const print = (...a) => console.log(...a)
const dump = o => console.dir(o,{depth:null})
const trace = x => (dump(x),x)
const str = o => JSON.stringify(o, null, '  ')
const eq = (x, y) => str(x) === str(y)
const dict = (kx, vx) => kx.reduce((d,k,i) => (d[k]=vx[i],d), {})
const fail = (msg, o) => { throw new Error(msg + ' ' + str(o)) }
const op2s = '=> == != <= >= || && + - * / < >'.split(' ')
const syms = '( ) [ ] ='.split(' ')

const tokenize = src => {
  let line=1
  let indent=0
  let left = src
  const tokens = []
  const match = r => (m => m ? m[0] : m)(left.match(r))
  const tags = (tag,code) => code ? ({code,[tag]:true,line,indent}) : null
  let space = false
  while (left) {
    const token = tags('space', match(/^[ \n]+/)) ||
      tags('dot', left[0] === '.' ? '.' : null) ||
      tags('op2', op2s.find(s => left.startsWith(s))) ||
      tags('sym', syms.find(s => left.startsWith(s))) ||
      tags('str', match(/^(["'`]).*?\1/)) ||
      tags('id', match(/^[a-zA-Z_]+/)) ||
      tags('num', match(/^[0-9]+/)) ||
      fail('tokenize', {left, src})
    if (token.space) {
      const brs=token.code.split('\n')
      if (brs.length >= 2) {
        line+=brs.length-1
        indent=brs[brs.length-1].length
      }
    } else {
      if (token.code === '=') {
        let p = tokens.length - 1
        while (p >= 0 && tokens[p].id && tokens[p].line === line) {
          p-=1
        }
        tokens[p+1].define = true
      } else if (token.code === '(' && !space) {
        token.call = true
      }
      tokens.push(token)
    }
    left = left.slice(token.code.length)
    space = token.space
  }
  return tokens.filter(t => !t.space)
}

const parse = tokens => {
  let pos = 0
  const nodes = []
  const consume = () => tokens[pos++] || fail('EOF', {pos,nodes,tokens})
  const check = f => tokens[pos] && f(tokens[pos])
  const drop = code => consume().code === code || fail('unexpected token', tokens[pos-1])
  const until = (f,g) => {
    const list = []
    while (tokens[pos] && g(tokens[pos])) {
      list.push(f())
    }
    return list
  }
  const nest = a => a.length === 1 ? a[0] : ({ary:a})
  const top = () => {
    if (check(t => t.define)) {
      const {line, indent} = tokens[pos]
      const args = until(consume, t => t.line === line && t.id)
      drop('=')
      if (check(t => t.line > line && t.indent > indent)) {
        return {ary: [{code:'='}, ...args, nest([{code: 'do'},...until(exp, t => t.indent > indent)])]}
      } else {
        return {ary: [{code:'='}, ...args, nest(until(exp, t => t.line === line))]}
      }
    } else {
      return exp()
    }
  }
  const exp = () => combine(unit())
  const combine = node => {
    if (check(t => t.dot)) {
      const dot = unit()
      const id = consume()
      return combine({ary: [dot, node, id]})
    }
    if (check(t => t.call)) {
      drop('(')
      node = {ary: [node, ...until(exp, t => t.code !== ')')]}
      drop(')')
      return combine(node)
    }
    if (check(t => t.op2)) {
      return combine(({ary: [unit(), node, exp()]}))
    }
    return node
  }
  const unit = () => {
    let node = consume()
    if (node.code === '(') {
      node = nest(until(top, t => t.code !== ')'))
      drop(')')
    } else if (node.code === '[') {
      node = {ary: [{code: 'list'}, ...until(top, t => t.code !== ']')]}
      drop(']')
    }
    return node
  }
  const strip = t => t.ary ? ({ary: t.ary.map(strip)}) : ({code: t.code})
  const unnest = t => t.ary && t.ary.length === 1 ? unnest(t.ary[0]) : t
  while (tokens[pos]) {
    nodes.push(strip(unnest(top())))
  }
  return nodes
}

const showType = top => {
  const show = t => {
    if (t.instance) {
      return show(t.instance)
    } else if (t.name) {
      return t.name + (t.params ? '(' + t.params.map(showType).join(' ') + ')' : '')
    } else if (t.types) {
      return '(' + t.types.map(show).join(' ') + ')'
    } else {
      return `failed to show ${str({t,top})}`
    }
  }
  const s = show(top)
  const o = {}
  const r = s.replace(/\d+/g, t => o[t]||=Object.keys(o).length+1)
  return r
}
const showTypeWithoutGenerics = t => showType(t).replace(/\(.+/, '')

const showNode = o => {
  const show = o => {
    if (typeof o === 'object' && o.constructor === Array) {
      if (o.length === 0) {
        return '()'
      } else {
        if (o[0].code === '=') {
          return '(= ' + o.slice(1).map(show).join(' ') + ')'
        } else {
          return '(' + o.map(show).join(' ') + ')'
        }
      }
    } else {
      if (!o.type) { fail('untyped', o) }
      return o.code + ':' + showType(o.type)
    }
  }
  return show(o)
}

const infer = (nodes,src) => {
  let tvarId = 0
  const tvar = () => (name => ({name,var:true}))((++tvarId).toString())
  const tgen = (name,...params) => ({name,params})
  const tlambda = (...types) => ({types:types})
  const ttype = (name) => ({name})
  const tint = ttype('int')
  const tstr = ttype('str')
  const tbool = ttype('bool')
  const tnil = ttype('nil')
  const topt = a => tgen('option', a)
  const tlist = a => tgen('list', a)
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const p = prune(t)
      return p.var ? (nonGeneric.includes(p.name) ? p : d[p.name]||=tvar()) :
        p.types ? ({types: p.types.map(rec)}) :
        p.params ? ({name: p.name, params: p.params.map(rec)}) :
        p
    }
    return rec(type)
  }
  const unify = (a, b) => {
    a = prune(a)
    b = prune(b)
    if (a.var) {
      if (a.name !== b.name) {
        a.instance = b
      }
    } else if (b.var) {
      unify(b, a)
    } else {
      if (a.name !== b.name) { fail(`type miss match`, {a:showType(a),b:showType(b),src,nodes}) }
      if (a.types || b.types) {
        if (a.types.length === b.types.length) {
          a.types.map((t,i) => unify(t, b.types[i]))
        } else {
          if (a.types.length > b.types.length) { [a,b] = [b,a] }
          const at = a.types.slice(-1)[0]
          if (at.var) {
            a.types.slice(0, -1).map((t,i) => unify(t, b.types[i]))
            unify(at, {types: b.types.slice(a.types.length - 1)})
          } else {
            fail('types miss match', {a,b})
          }
        }
      }
      if (a.params || b.params) {
        if (a.params.length === b.params.length) {
          a.params.map((t,i) => unify(t, b.params[i]))
        } else {
          fail('params miss match', {a,b})
        }
      }
    }
  }
  const v1 = tvar()
  const v2 = tvar()
  const methods = {
    str: (self) => ({
      at: tlambda(self, tint, tstr),
    }),
    int: (self) => ({
      neg: tlambda(self, tint),
      abs: tlambda(self, tint)
    }),
    list: (self) => ({
      size: tlambda(self, tint),
      get: tlambda(self, tint, topt(self.params[0]))
    }),
    option: (self) => ({
      map: tlambda(self, tlambda(self.params[0], v1), topt(v1)),
      alt: tlambda(self, self.params[0], topt(self.params[0])),
    })
  }
  let env = {
    __cache: {},
    'true': tbool,
    'false': tbool,
    '+': tlambda(tint, tint, tint),
    '-': tlambda(tint, tint, tint),
    '*': tlambda(tint, tint, tint),
    '/': tlambda(tint, tint, tint),
    '<': tlambda(tint, tint, tbool),
    '>': tlambda(tint, tint, tbool),
    '<=': tlambda(tint, tint, tbool),
    '>=': tlambda(tint, tint, tbool),
    '==': tlambda(v1, v1, tbool),
    '!=': tlambda(v1, v1, tbool),
    '||': tlambda(tbool, tbool, tbool),
    '&&': tlambda(tbool, tbool, tbool),
    'if': tlambda(tbool, v1, v1, v1),
    'some': tlambda(v1, tgen('option', v1)),
    'error': tlambda(v1, tgen('option', v2)),
    'none': tgen('option', v1),
  }
  const local = (node, d, nonGeneric) => {
    const keys = Object.keys(d)
    const bk = {}
    keys.map(k => (bk[k]=env[k], env[k]=d[k]))
    const ret = analyse(node, nonGeneric)
    keys.map(k => env[k] = bk[k])
    return ret
  }
  const prune = t => (t.var && t.instance) ? t.instance = prune(t.instance) : t
  const analyse = (node, nonGeneric) => node.type = _analyse(node, nonGeneric)
  const _analyse = (node, nonGeneric) => {
    if (node.ary) {
      const ary = node.ary
      if (ary.length === 0) { return tnil }
      let [head,...tail] = ary
      if (head.code === '=') {
        if (tail.length < 2) { fail('Not enough argument', {tail,ary}) }
        const name = tail[0].code
        if (tail.length === 2) {
          return tail[0].type = env[name] = analyse(tail[1], nonGeneric)
        } else {
          const args = tail.slice(1,-1)
          const argt = args.map(_ => tvar())
          args.map((a,i) => a.type = argt[i])
          const body = tail.slice(-1)[0]
          const d = dict(args.map(t => t.code), argt)
          const ft = tlambda(...argt, local(body, d, nonGeneric.concat(argt.map(t=>t.name))))
          return tail[0].type = env[name] = ft
        }
      } else if (head.code === '=>') {
        const args = tail.slice(0, -1)
        const argt = args.map(a => a.type = tvar())
        const body = tail.slice(-1)[0]
        const d = dict(args.map(t => t.code), argt)
        const ft = tlambda(...argt, local(body, d, nonGeneric.concat(argt.map(t=>t.name))))
        return body.type = ft
      } else if (head.code === '.') {
        const [lhs, rhs] = tail
        const args = [lhs].concat(tail.slice(2))
        const rt = tvar()
        const type = prune(analyse(lhs, nonGeneric))
        const ft = methods[showTypeWithoutGenerics(type)](type)[rhs.code]
        unify(tlambda(...args.map(t => analyse(t, nonGeneric)), rt), ft)
        return head.type = rt
      } else if (head.code === 'list') {
        if (tail.length === 0) {
          return head.type = tlist
        } else {
          const types = tail.map(t => analyse(t, nonGeneric))
          return head.type = tgen('list', types.reduce((a,b) => (unify(a,b),a)))
        }
      } else if (head.code === 'do') {
        let t = topt(tvar())
        for (const node of tail) {
          unify(t, analyse(node, nonGeneric))
        }
        return t
      } else if (tail.length) {
        const argv = tail.map(t => analyse(t, nonGeneric))
        const ckey = str(argv)
        const rt = (env.__cache[ckey] ||= tvar())
        const ft = analyse(head, nonGeneric)
        unify(tlambda(...argv, rt), ft)
        return rt
      } else {
        return head.type = analyse(head, nonGeneric)
      }
    } else {
      if (node.code === 'list') {
        return tgen('list', tvar())
      } else if (node.code.match(/^[0-9]+/)) {
        return tint
      } else if (node.code.match(/^["'`]/)) {
        return tstr
      } else if (env[node.code]) {
        return fresh(env[node.code], nonGeneric) || fail(`Not found ${v}`, node)
      } else {
        fail(`unknown "${str(node)}"`, {src,node,env:Object.keys(env)})
      }
    }
  }

  // reserve for circulated reference
  for (const node of nodes) {
    if (node.ary && node.ary[0].code === '=') {
      env[node.ary[1].code] = tvar()
    }
  }
  // type inference for each node
  return nodes.map(node => analyse(node, []))
}

const __int_neg = x => -1 * x
const __list_size = x => x.length
const __str_at = (s,i) => i < s.length ? s[i] : new err('out of index')
const __option_map = (o,f) => o.__type === 'error' ? o : f(o)
const __option_alt = (o,v) => o.__type === 'error' ? v : o

const compile = nodes => {
  const removeGenerics = t => showType(t).replace(/\(.+/, '')
  const monadic = node => {
    let line = js(node)
    if (line.startsWith('const ')) {
      line = line.replace('=', '= __ =')
    } else {
      line = '__ = ' + line
    }
    return line + '; if (__.__type === "error") { return __ }'
  }
  const js = node => {
    const ary = node.ary
    const type = showType(node.type)
    if (ary) {
      if (ary.length === 0) {
        fail('empty array', nodes)
      }
      if (ary[0].code === 'list') {
        return '[' + ary.slice(1).map(js).join(',') + ']'
      } else if (ary[0].code === 'do') {
        if (ary.length === 2) {
          return js(ary[1])
        } else {
          return '(() => {' + ary.slice(1).map(monadic).join('\n') + '\nreturn __})()'
        }
      } else if (ary.length === 1) {
        return js(ary[0])
      } else if (ary[0].code === '=') {
        const name = ary[1].code
        const args = ary.slice(2, -1).map(t => t.code)
        const body = ary[node.ary.length - 1]
        if (args.length) {
          return `const ${name} = (${args.join(',')}) => ${js(body)}`
        } else {
          return `const ${name} = ${js(body)}`
        }
      } else if (ary[0].code === '.') {
        return `__${removeGenerics(ary[1].type)}_${ary[2].code}(${js(ary[1])})`
      } else if (op2s.includes(ary[0].code)) {
        return js(ary[1]) + ary[0].code + js(ary[2])
      } else {
        const args = ary.slice(1).map(js)
        if (ary[0].code === 'if') {
          const [cond,lhs,rhs] = args
          return `${cond} ? ${lhs} : ${rhs}`
        } else if (ary[0].code === 'some' && args.length === 1) {
          return args[0]
        } else if (ary[0].code === 'error') {
          return `{message: ${args[0]}, __type: 'error'}`
        } else {
          return `${js(ary[0])}(${args.join(',')})`.replace(')(', ',') // TODO: type safe implement for uncarry
        }
      }
    } else if (['int', 'str', 'bool'].includes(type)) {
      return node.code
    } else {
      return node.code
    }
  }
  return nodes.map(js).join('\n')
}

function testType() {
  const run = src => {
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    try {
      const types = infer(nodes, src)
      return {tokens, nodes, types}
    } catch (e) {
      e.nodes = nodes
      throw e
    }
  }
  const reject = src => {
    try {
      run(src)
      print('Failed')
      print('src:', src)
    } catch (e) {
      process.stdout.write('.')
    }
  }
  const inf = (src, expect) => {
    try {
      let result = run(src)
      const actual = showType(result.types.slice(-1)[0])
      if (eq(actual, expect)) {
        process.stdout.write('.')
      } else {
        print('Failed')
        print('expect:', expect)
        print('actual:', actual)
        print('   src:', src)
        print('tokens:', result.tokens)
        write(' nodes: ')
        dump(result.nodes)
      }
    } catch (e) {
      print('Failed')
      print('  src:', src)
      print('error:', e)
      if (e.nodes) {
        write('nodes:')
        dump(e.nodes)
      }
    }
  }

  // primitives
  inf('1', 'int')
  inf('true', 'bool')
  inf('false', 'bool')
  inf('"hi"', 'str')

  // containers
  inf('[]', 'list(1)')
  inf('[1]', 'list(int)')
  inf('[1 2]', 'list(int)')
  inf('[true false]', 'list(bool)')

  // function
  inf('id x = x\nid(1)', 'int')
  inf('id x = x\nid 1', 'int')
  inf('x => x', '(1 1)')

  // methods
  inf('1.neg', 'int')
  inf('1.abs', 'int')
  inf('[1].size', 'int')
  inf('[1].get(0)', 'option(int)')

  // option
  inf('some(1)', 'option(int)')
  inf('none', 'option(1)')
  inf('some(1).map(x => x)', 'option(int)')
  inf('some(1).alt(2)', 'option(int)')

  // embedded
  inf('1 + 1', 'int')
  inf('1 < 1', 'bool')
  inf('if true true true', 'bool')
  inf('if true 1 1', 'int')

  // define
  inf('f = 1\nf', 'int')
  inf('f a = a\nf 1', 'int')
  inf('f a b = a + b\nf 1 2', 'int')

  // generics
  inf('f a = a', '(1 1)')
  inf('f a b = a', '(1 2 1)')
  inf('f a b = b', '(1 2 2)')
  inf('f a = a\nf 1\nf true', 'bool')
  inf('f x = x\nif (f true) (f 1) (f 2)', 'int')

  // type inference
  inf('f x = x + 1\ng x = x + 2\nf(1) + g(1)', 'int')

  inf('_ f g x = g(f(x))', '((1 2) (2 3) 1 3)')
  inf('_ x y z = x z (y z)', '((1 2 3) (1 2) 1 3)')
  inf('_ b x = if(x(b) x (x => b))', '(1 (1 bool) (1 1))')
  inf('_ x = if true x (if x true false)', '(bool bool)')
  inf('_ x y = if x x y', '(bool bool bool)')
  inf('_ n = (x => x(y => y))(f => f n)', '(1 1)')
  inf('_ x y = x y', '((1 2) 1 2)')
  inf('_ x y = x (y x)', '((1 2) ((1 2) 1) 2)')
  inf('_ h t f x = f h (t f x)', '(1 ((1 2 3) 4 2) (1 2 3) 4 3)')
  inf('_ x y = x (y x) (y x)', '((1 1 2) ((1 1 2) 1) 2)')
  inf('id x = x\nf y = id(y(id))', '(((1 1) 2) 2)')
  inf('id x = x\nf y = id (y id)', '(((1 1) 2) 2)')
  inf('f x = x\ng = if (f true) (f 1) (f 2)', 'int')
  inf('f x = 3\ng = (f true) + (f 4)', 'int')
  inf('f x = x\ng y = y\nh b = if b (f g) (g f)', '(bool (1 1))')

  // recursive
  inf('f x = (f x)', '(1 2)')
  inf('f n = (g n)\ng n = (f n)', '(1 2)')

  // implicit curring
  inf('f a b = a + b\ng = f 1', '(int int)')

  // monadic statement
  inf('f =\n  some(1)', 'option(int)')
  inf('f =\n  error("hi")\n  some(1)', 'option(int)')

  // type errors
  reject('(+ 1 true)')
  reject('[1 false]')
}

const testJs = () => {
  const run = src => {
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    let js, value
    try {
      infer(nodes, src)
      js = compile(nodes)
      value = eval(js + '\ntypeof main === "function" ? main() : main')
    } catch (e) {
      value = e
    }
    return {js, value}
  }
  const t = (expect, src) => {
    const {js, value} = run(src)
    if (eq(expect, value)) {
      process.stdout.write('.')
    } else {
      print('Failed')
      print('expect:', expect)
      print('actual:', value)
      print('src   :', src)
      print('js    :', js)
    }
  }

  // primitives
  t(1, 'main = 1')
  t('hi', 'main = "hi"')
  t(true, 'main = true')

  // containers
  t([], 'main = []')
  t([1], 'main = [1]')
  t([1, 2], 'main = [1 2]')

  // operators
  t(-3, 'main = 1 + 2 - 3 * 4 / 2')
  t(true, 'main = (1 == 2) || (3 != 4)')

  // control flow
  t(1, 'main = if true 1 main', 'bool')
  t(2, 'main = if false main 2', 'bool')

  // functions
  t(3, 'f = 1\nmain = f + 2')
  t(3, 'f a = a + 2\nmain = f 1')

  // methods
  t(-1, 'main = 1.neg')
  t(1, 'main = [1].size')

  // generics
  t('i', 'id a = a\nmain = id("hi").at(id(1))')

  // recursive
  t(120, 'f n = if (n > 1) (n * (f n - 1)) 1\nmain = f(5)')

  // TODO: implicit curring
  //t(7, 'add a b = a + b\ninc a = add 1\nmain = (inc 1) + (add 2 3)')

  // option
  t(1, 'main = some(1)')
  t(3, 'main = (some 1).map(v => (v + 2))')
  t(1, 'main = (error "hi").alt(1)')

  // monadic statement
  t(1, 'main =\n  some(1)')
  t(2, 'main =\n  some(1)\n  some(2)')
  t({message: 'hi', __type: 'error'}, 'main =\n  error("hi")\n  some(2)')
}

testType()
testJs()
print('ok')
