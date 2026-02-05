package cs.system;

@:native("System.Func`1")
extern class Func_1<TResult> extends cs.system.MulticastDelegate {
	function new(func:()->TResult):Void;
	function Invoke():TResult;
}
