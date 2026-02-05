package cs.system;

@:native("System.Func`12")
extern class Func_12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5, arg6:T6, arg7:T7, arg8:T8, arg9:T9, arg10:T10, arg11:T11)->TResult):Void;
	function Invoke(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5, arg6:T6, arg7:T7, arg8:T8, arg9:T9, arg10:T10, arg11:T11):TResult;
}
