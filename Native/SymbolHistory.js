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

	var url = 'http://bridge-dev.srv.robofx.com/ohlc/demo-dev';
	
	function randomHex(length) {
	    var base = Math.pow(16, length - 1);
	    var range = Math.pow(16, length) - base - 1;
	    return (Math.round(Math.random() * range) + base).toString(16);
	}

	function get(symbol, year, period, from, to) {
		return new Promise(function(resolve, reject) {
			var cbName = 'HistoryCallback_' + randomHex(6);
			var reqUrl = [url, year, symbol, period, 'bmv'].join('/') + '?jsonp=' + cbName + '&from=' + from + '&to=' + to;
			window[cbName] = function(result) {
				delete window[cbName];
				tag.parentNode.removeChild(tag);
				resolve(result);
			}
			var tag = document.createElement('script');
			tag.type = "text/javascript";
			tag.src = reqUrl;
			document.querySelector('head').appendChild(tag);
		})
	}

	function load(symbol, year, period, from, to) {
		return Task.asyncFunction(function(callback) {
			get(symbol, year, period, from, to).then(function(result) {
				callback(Task.succeed(JSON.stringify(result)))
			})
		});
	}

	var getCurrentTime = Task.asyncFunction(function(callback) {
		return callback(Task.succeed(Date.now()));
	});

	return localRuntime.Native.SymbolHistory.values = {
		load: curry(load),
		getCurrentTime: getCurrentTime
	};
};