package cs.system;

/** Encapsulates a method that has no parameters and does not return a value. */
@:native("System.Action`1")
extern class Action_1<T> extends cs.system.MulticastDelegate {
	function new(func:(obj:T)->Void):Void;
	function Invoke(obj:T):Void;
}
