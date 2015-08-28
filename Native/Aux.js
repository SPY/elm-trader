Elm.Native.Aux = {};
Elm.Native.Aux.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Aux = localRuntime.Native.Aux || {};
	if (localRuntime.Native.Aux.values)
	{
		return localRuntime.Native.Aux.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	var getCurrentTime = Task.asyncFunction(function(callback) {
		return callback(Task.succeed(Date.now()));
	});

	var dimensions = Task.asyncFunction(function(callback) {
		return callback(Task.succeed(Utils.Tuple2(window.innerWidth, window.innerHeight)));
	});

	return localRuntime.Native.Aux.values = {
		getCurrentTime: getCurrentTime,
		dimensions: dimensions
	};
};