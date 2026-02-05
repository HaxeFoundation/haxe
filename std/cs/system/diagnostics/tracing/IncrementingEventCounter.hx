package cs.system.diagnostics.tracing;

@:native("System.Diagnostics.Tracing.IncrementingEventCounter")
extern class IncrementingEventCounter extends cs.system.diagnostics.tracing.DiagnosticCounter {
	var DisplayRateTimeScale(default, default):cs.system.TimeSpan;
	function new(name:String, eventSource:cs.system.diagnostics.tracing.EventSource):Void;
	function Increment(?increment:Float):Void;
	function ToString():String;
}
