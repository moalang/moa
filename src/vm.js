const _ = {}
function _if(...args) {
  assert(args.length % 2 === 1, args)
  for (let i=1; i<args.length; i+=2) {
    if (args[i-1].valueOf()) {
      return args[i].valueOf()
    }
  }
  return args[args.length-1]
}
function _div(lhs, rhs) {
  if (rhs === 0) {
    return error('divide by zero', lhs, rhs)
  }
  return lhs / rhs
}
function _type(line) {
  if (line.startsWith(': ')) {
    const keys = line.slice(1).split(',').map(x => x.trim().split(' ')[0])
    return (...vals) => keys.reduce((o,k,i) => ({...o, [k]: vals[i]}), {})
  } else if (line.startsWith(':| ')) {
    const f = (x,...args) => args[x.index](x.val)
    const fields = line.slice(2).split('|').map(f => f.trim())
    for (const [index, field] of fields.entries()) {
      if (field.match(/^\w+$/)) {
        f[field] = {index}
      } else if (field.match(/^\w+ \w+/)) {
        const tag = field.split(' ')[0]
        f[tag] = val => ({index,val})
      } else if (field.includes(':')) {
        const [tag, names] = field.split(': ')
        const keys = names.split(',').map(x => x.trim().split(' ')[0])
        f[tag] = (...vals) => ({index, val:keys.reduce((o,k,i) => ({...o, [k]: vals[i]}), {})})
      } else {
        assert(false, line)
      }
    }
    return f
  } else {
    assert(false, line)
  }
}
function _var(val) {
  this.val = val
}
_var.prototype.eff = function(op, rhs) {
  rhs = rhs.valueOf()
  switch(op) {
    case '+=': return () => this.val += rhs
    case '-=': return () => this.val -= rhs
    case '*=': return () => this.val *= rhs
    case '/=': return () => this.val = _div(this.val, rhs)
    default:
      assert(false, {op, lhs: this, rhs})
  }
}
_var.prototype.valueOf = function() {
  return this.val
}
_var.prototype.isEff = true
function error(message, args) {
  try {
    throw new Error()
  } catch (e) {
    const eid = e.stack
    const obj = {message, args, eid}
    obj.catch = (target,alt) => target.valueOf().eid === eid ? alt : obj
    obj.valueOf = () => obj
    obj.if = cond => cond ? obj : 'error_if_pass'
    obj.unless = cond => !cond ? obj : 'error_unless_pass'
    return obj
  }
}
function _bind(obj, f) {
  obj = obj.valueOf()
  if (obj.eid) {
    return obj
  }
  return f(obj)
}
function _op2(op, lhs, rhs) {
  lhs = lhs.valueOf()
  rhs = rhs.valueOf()
  switch (op) {
    case '+' : return lhs + rhs
    case '-' : return lhs - rhs
    case '*' : return lhs * rhs
    case '/' : return _div(lhs, rhs)
    case '++' : return lhs.concat(rhs)
    case '==' : return lhs === rhs
    case '!=' : return lhs !== rhs
    case '<=' : return lhs <= rhs
    case '>=' : return lhs >= rhs
    case '&&' : return lhs && rhs
    case '||' : return lhs || rhs
    case '<' : return lhs < rhs
    case '>' : return lhs > rhs
    default:
      assert(false, {op,lhs,rhs})
  }
}
function _top(obj) {
  obj = obj.valueOf()
  return obj.eid ? 'error: ' + obj.message : obj
}

// WARN: global pollution
Object.prototype.catch = function() { return this }
String.prototype.__defineGetter__('int', function() { return parseInt(this) })
String.prototype.__defineGetter__('len', function() { return this.length })
Number.prototype.__defineGetter__('string', function() { return this.toString() })
Array.prototype.__defineGetter__('len', function() { return this.length })
Function.prototype.valueOf = function() {
  let f = this
  while (typeof(f) === 'function') {
    f = f()
  }
  return f
}
Function.prototype.catch = function (e, alt) {
  e = e.valueOf()
  const f = this
  return (...args) => {
    const ret = f(...args).valueOf()
    const v = ret.eid === e.eid ? alt : ret
    return v
  }
}
