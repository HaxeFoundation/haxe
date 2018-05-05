package flash.profiler;

extern class Telemetry {
	function new() : Void;
	static var connected(default,never) : Bool;
	static var spanMarker(default,never) : Float;
	static function registerCommandHandler(commandName : String, handler : Dynamic) : Bool;
	static function sendMetric(metric : String, value : Dynamic) : Void;
	static function sendSpanMetric(metric : String, startSpanMarker : Float, ?value : Dynamic) : Void;
	static function unregisterCommandHandler(commandName : String) : Bool;
}
