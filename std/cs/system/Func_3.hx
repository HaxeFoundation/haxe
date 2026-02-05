package cs.system;

@:native("System.Func`3")
extern class Func_3<T1, T2, TResult> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2)->TResult):Void;
	function Invoke(arg1:T1, arg2:T2):TResult;
}
