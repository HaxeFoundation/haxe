package cs.system;

/** Encapsulates a method that has no parameters and does not return a value. */
@:native("System.Action`3")
extern class Action_3<T1, T2, T3> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2, arg3:T3)->Void):Void;
	function Invoke(arg1:T1, arg2:T2, arg3:T3):Void;
}
