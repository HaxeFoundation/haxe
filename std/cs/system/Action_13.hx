package cs.system;

/** Encapsulates a method that has no parameters and does not return a value. */
@:native("System.Action`13")
extern class Action_13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5, arg6:T6, arg7:T7, arg8:T8, arg9:T9, arg10:T10, arg11:T11, arg12:T12, arg13:T13)->Void):Void;
	function Invoke(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5, arg6:T6, arg7:T7, arg8:T8, arg9:T9, arg10:T10, arg11:T11, arg12:T12, arg13:T13):Void;
}
