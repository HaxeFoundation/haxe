package cs.system.diagnostics.tracing;

@:native("System.Diagnostics.Tracing.DiagnosticCounter")
extern class DiagnosticCounter {
	var DisplayName(default, default):String;
	var DisplayUnits(default, default):String;
	var EventSource(default, never):cs.system.diagnostics.tracing.EventSource;
	var Name(default, never):String;
	function AddMetadata(key:String, value:String):Void;
	function Dispose():Void;
}
