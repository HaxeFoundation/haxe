package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.StrongBox")
extern class StrongBox<T> {
	var Value:T;
	@:overload(function():Void {})
	function new(value:T):Void;
}
