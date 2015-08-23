var Quotes = (function() {
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
	var port;
	return {
		default: function(ports) {
			ports.quotes = ['notconnected', null];
		},
		init: function(app) {
			port = app.ports.quotes;
			this.connect();
		},
		connect: function() {
			connection = new WebSocket('ws://bridge-dev.srv.robofx.com/demo-dev/', 'SnapshotFullRefresh2')
			connection.addEventListener('open', function() {
				connection.send(JSON.stringify(handShakeMessage));
				port.send(['open', null])
			})
			connection.addEventListener('message', function(message) {
				port.send(['message', message.data])
			})
			connection.addEventListener('close', function() {
				port.send(['close', null])
			})
		},
		disconnect: function() {
			connection.close();
		}
	}
})()