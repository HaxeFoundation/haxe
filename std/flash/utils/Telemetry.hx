package flash.utils;

@:require(flash11_2) extern class Telemetry extends flash.events.EventDispatcher {
	var bufferLength : UInt;
	var connected(default,null) : Bool;
	function new() : Void;
	function enableMetric(metric : String, enable : Bool) : Void;
	function flush() : Void;
	function isMetricEnabled(metric : String) : Bool;
	function registerMethod(functionId : String, f : Dynamic) : Void;
	function sendMetric(metric : String, value : Dynamic) : Void;
	function sendSpanMetric(metric : String, startMarker : Float) : Void;
	static var marker(default,null) : Float;
	static var telemetry(default,null) : Telemetry;
}
