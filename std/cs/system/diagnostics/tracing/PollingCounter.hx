package cs.system.diagnostics.tracing;

@:native("System.Diagnostics.Tracing.PollingCounter")
extern class PollingCounter extends cs.system.diagnostics.tracing.DiagnosticCounter {
	function new(name:String, eventSource:cs.system.diagnostics.tracing.EventSource, metricProvider:cs.system.Func_1<Float>):Void;
	function ToString():String;
}
