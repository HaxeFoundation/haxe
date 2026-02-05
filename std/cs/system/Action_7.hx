package cs.system;

/** Encapsulates a method that has no parameters and does not return a value. */
@:native("System.Action`7")
extern class Action_7<T1, T2, T3, T4, T5, T6, T7> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5, arg6:T6, arg7:T7)->Void):Void;
	function Invoke(arg1:T1, arg2:T2, arg3:T3, arg4:T4, arg5:T5, arg6:T6, arg7:T7):Void;
}
