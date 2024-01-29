String.prototype.has = function(s) { return this.indexOf(s) !== -1 }
Array.prototype.has = function(s) { return this.includes(s) }
Object.defineProperty(Array.prototype, 'sum', {get: function() { return this.reduce((acc, v) => acc + v, 0) }})
Object.defineProperty(Array.prototype, 'count', {get: function() { return this.length }})
Object.prototype.or = function(_) { return this }
global.__pure = x => typeof(x) === 'function' && x.length == 0 && !x.__stmt ? __pure(x()) : x
global.__effect = x => typeof(x) === 'function' && x.length == 0 ? __effect(x()) : x
global.ok = x => x
global.err = x => { return {__err: x, toString: () => x, or: y => y} }
global.fail = x => { thorw(new Error(x)) }
