package cs.system;

/** Encapsulates a method that has no parameters and does not return a value. */
@:native("System.Action`2")
extern class Action_2<T1, T2> extends cs.system.MulticastDelegate {
	function new(func:(arg1:T1, arg2:T2)->Void):Void;
	function Invoke(arg1:T1, arg2:T2):Void;
}
