package cs.system;

@:native("System.Func`6")
extern class Func_6<T1, T2, T3, T4, T5, TResult> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5)->TResult):Void;
	function Invoke(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5):TResult;
}
