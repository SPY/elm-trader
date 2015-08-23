Elm.Native.SymbolHistory = {};
Elm.Native.SymbolHistory.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.SymbolHistory = localRuntime.Native.SymbolHistory || {};
	if (localRuntime.Native.SymbolHistory.values)
	{
		return localRuntime.Native.SymbolHistory.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function curry(fn, ctx, length) {
		var len = fn.length || length;
		return function() {
			var args = [].slice.call(arguments);
			if (args.length >= len) {
				return fn.apply(ctx || null, args);
			}
			else {
				return function cont() {
					args = args.concat([].slice.call(arguments));
					if (args.length >= len) {
						return fn.apply(ctx || null, args);
					}
					else {
						return cont;
					}
				};
			}
		}
	}

	function load(symbol, year, period, from, to) {
		return Task.asyncFunction(function(callback) {
			History.get(symbol, year, period, from, to).then(function(result) {
				callback(Task.succeed(JSON.stringify(result)))
			})
		});
	}

	function log(string)
	{
		return Task.asyncFunction(function(callback) {
			console.log(string);
			return callback(Task.succeed(Utils.Tuple0));
		});
	}

	return localRuntime.Native.SymbolHistory.values = {
		load: curry(load),
		log: log
	};
};