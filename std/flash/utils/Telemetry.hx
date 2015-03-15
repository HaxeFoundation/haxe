package flash.utils;

@:require(flash11_4) @:native("flash.profiler.Telemetry") extern class Telemetry extends flash.events.EventDispatcher {
	static var connected(default,null) : Bool;
	static var spanMarker(default,null) : Float;
	static function registerCommandHandler(commandName : String, handler : Dynamic) : Bool;
	static function unregisterCommandHandler(commandName : String) : Bool;
	static function sendMetric(metric : String, value : Dynamic) : Void;
	static function sendSpanMetric(metric : String, startMarker : Float) : Void;
}