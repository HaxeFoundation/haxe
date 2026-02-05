package cs.system.diagnostics.tracing;

/** Provides the ability to collect statistics for very frequent events through the   class. */
@:native("System.Diagnostics.Tracing.EventCounter")
extern class EventCounter extends cs.system.diagnostics.tracing.DiagnosticCounter {
	function new(name:String, eventSource:cs.system.diagnostics.tracing.EventSource):Void;
	function ToString():String;
	@:overload(function(value:Float):Void {})
	/**
	 * Writes the metric if performance counters are on.
	 * @param value The value to be written.
	 */
	function WriteMetric(value:Single):Void;
}
