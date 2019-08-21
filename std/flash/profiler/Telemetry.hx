package flash.profiler;

extern class Telemetry {
	function new() : Void;
	@:flash.property static var connected(get,never) : Bool;
	@:flash.property static var spanMarker(get,never) : Float;
	private static function get_connected() : Bool;
	private static function get_spanMarker() : Float;
	static function registerCommandHandler(commandName : String, handler : Dynamic) : Bool;
	static function sendMetric(metric : String, value : Dynamic) : Void;
	static function sendSpanMetric(metric : String, startSpanMarker : Float, ?value : Dynamic) : Void;
	static function unregisterCommandHandler(commandName : String) : Bool;
}
