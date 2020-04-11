String.prototype.has = function(s) { return this.indexOf(s) !== -1 }
Array.prototype.has = function(s) { return this.includes(s) }
Object.defineProperty(Array.prototype, 'count', {get: function() { return this.length }})
global.__eval = x => typeof(x) === 'function' && x.length == 0 ? __eval(x()) : x
