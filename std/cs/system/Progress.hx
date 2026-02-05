package cs.system;

@:native("System.Progress")
extern class Progress<T> {
	@:overload(function():Void {})
	function new(handler:cs.system.Action_1<T>):Void;
}
