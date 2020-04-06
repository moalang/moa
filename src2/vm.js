Object.defineProperty(Array.prototype, 'count', {get: function() { return this.length }})
global.__eval = x => typeof(x) === 'function' ? __eval(x()) : x
