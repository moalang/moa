// pure values
class Pure {
  constructor(val) {
    this.val = val
  }
  op2(op, rhsObject) {
    const lhs = this.val
    const rhs = rhsObject.val
    switch (op) {
      case '+' : return new Int(lhs + rhs)
      case '-' : return new Int(lhs - rhs)
      case '*' : return new Int(lhs * rhs)
      case '.' : return new Str(lhs.concat(rhs))
      case '++' : return new Ary(lhs.concat(rhs))
      case '==' : return new Bool(lhs === rhs)
      case '!=' : return new Bool(lhs !== rhs)
      case '<=' : return new Bool(lhs <= rhs)
      case '>=' : return new Bool(lhs >= rhs)
      case '&&' : return new Bool(lhs && rhs)
      case '||' : return new Bool(lhs || rhs)
      case '<' : return new Bool(lhs < rhs)
      case '>' : return new Bool(lhs > rhs)
      default:
        fail(op, rhsObject)
    }
  }
  catch() { return this }
  then(f) { return f(this) }
  trace() { console.dir(this, {depth: null}); return this }
  unwrap() { return this.val }
}
class Int extends Pure {
  get string() { return this.val.toString() }
}
class Str extends Pure {
  get int() { return parseInt(this.val) }
  get len() { return this.val.length }
}
class Bool extends Pure {
}
class Ary extends Pure {
  get len() { return this.val.length }
}
class Func extends Pure {
  catch(e, alt) {
    return (...args) => {
      const ret = this.val(...args)
      return ret.constructor === Err ? alt : ret
    }
  }
  then(f) {
    return (...args) => {
      const ret = this.val(...args)
      return ret.constructor === Err ? f(ret) : ret
    }
  }
  unwrap() {
    return this.val.length === 0 ? this.val() : this.val
  }
}

// effect
class Effect extends Pure {
  modify(op, rhsObject) {
    const rhs = rhsObject
    switch(op) {
      case '+=': return () => { this.val += rhs; return this }
      case '-=': return () => { this.val -= rhs; return this }
      case '*=': return () => { this.val *= rhs; return this }
      default:
        fail(op, rhsObject)
    }
  }
}
class Err extends Pure {
  catch(err, alt) {
    return err.val === this.val ? alt : this
  }

  if(cond) {
    return cond ? this : new Str('pass_if')
  }

  unless(cond) {
    return cond ? this : new Str('pass_unless')
  }

  catch(err, alt) { return this.val === err.val ? alt : this }
  then() { return this }
  unwrap() {
    return "error: " + this.val
  }
}
