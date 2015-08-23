Elm.Native.Quotes = {};
Elm.Native.Quotes.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Quotes = localRuntime.Native.Quotes || {};
	if (localRuntime.Native.Quotes.values)
	{
		return localRuntime.Native.Quotes.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);
	var Signal = Elm.Native.Signal.make(localRuntime);

	var handShakeMessage = {
		"login":"demo-dev",
		"password_hash":"78bfcd9c99a6601ee7673459b1b49d79f984ddcd",
		"symbols":[
			"AUDCAD.m","AUDCHF.m","AUDJPY.m","AUDNZD.m","AUDUSD.m","CADCHF.m","CADJPY.m","CHFJPY.m",
			"EURAUD.m","EURCAD.m","EURCHF.m","EURGBP.m","EURJPY.m","EURNZD.m","EURTRY.m","EURUSD.m",
			"GBPAUD.m","GBPCAD.m","GBPCHF.m","GBPJPY.m","GBPNZD.m","GBPUSD.m","NZDCAD.m","NZDCHF.m",
			"NZDJPY.m","NZDUSD.m","USDCAD.m","USDCHF.m","USDCNH.m","USDJPY.m","USDMXN.m","USDRUB.m",
			"USDTRY.m","USDZAR.m","XAUUSD.m","XAGUSD.m","EURUSD.5Y","GBPUSD.5Y","USDCHF.5Y",
			"USDJPY.5Y"
		]
	}

	var connection;
	var quotes = Signal.input('Quotes.quotes', Utils.Tuple2('notconnected', null));
	function connect() {
		connection = new WebSocket('ws://bridge-dev.srv.robofx.com/demo-dev/', 'SnapshotFullRefresh2')
		connection.addEventListener('open', function() {
			connection.send(JSON.stringify(handShakeMessage));
			localRuntime.notify(quotes.id, Utils.Tuple2('open', null))
		})
		connection.addEventListener('message', function(message) {
			localRuntime.notify(quotes.id, Utils.Tuple2('message', message.data))
		})
		connection.addEventListener('close', function() {
			localRuntime.notify(quotes.id, Utils.Tuple2('close', null))
		})
	}
	connect()

	return localRuntime.Native.Quotes.values = {
		quotes: quotes
	};
};