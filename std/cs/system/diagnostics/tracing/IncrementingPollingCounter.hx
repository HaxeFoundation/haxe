package cs.system.diagnostics.tracing;

@:native("System.Diagnostics.Tracing.IncrementingPollingCounter")
extern class IncrementingPollingCounter extends cs.system.diagnostics.tracing.DiagnosticCounter {
	var DisplayRateTimeScale(default, default):cs.system.TimeSpan;
	function new(name:String, eventSource:cs.system.diagnostics.tracing.EventSource, totalValueProvider:cs.system.Func_1<Float>):Void;
	function ToString():String;
}
