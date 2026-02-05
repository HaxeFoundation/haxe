package cs.system.threading.tasks.sources;

@:native("System.Threading.Tasks.Sources.ManualResetValueTaskSourceCore")
extern class ManualResetValueTaskSourceCore<TResult> extends cs.system.ValueType {
	var RunContinuationsAsynchronously(default, default):Bool;
	var Version(default, never):cs.Int16;
	function GetResult(token:cs.Int16):TResult;
	function GetStatus(token:cs.Int16):cs.system.threading.tasks.sources.ValueTaskSourceStatus;
	function OnCompleted(continuation:cs.system.Action_1<Dynamic>, state:Dynamic, token:cs.Int16, flags:cs.system.threading.tasks.sources.ValueTaskSourceOnCompletedFlags):Void;
	function Reset():Void;
	function SetException(error:cs.system.Exception):Void;
	function SetResult(result:TResult):Void;
}
