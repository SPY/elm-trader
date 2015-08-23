var History = (function() {
	var url = 'http://bridge-dev.srv.robofx.com/ohlc/demo-dev';
	
	function randomHex(length) {
	    var base = Math.pow(16, length - 1);
	    var range = Math.pow(16, length) - base - 1;
	    return (Math.round(Math.random() * range) + base).toString(16);
	}

	return {
		get: function(symbol, year, period, from, to) {
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
	}
})()