package cs.system;

@:native("System.Func`4")
extern class Func_4<T1, T2, T3, TResult> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2, arg3:T3)->TResult):Void;
	function Invoke(arg1:T1, arg2:T2, arg3:T3):TResult;
}
