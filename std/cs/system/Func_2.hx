package cs.system;

@:native("System.Func`2")
extern class Func_2<T, TResult> extends cs.system.MulticastDelegate {
	function new(func:(arg:T)->TResult):Void;
	function Invoke(arg:T):TResult;
}
