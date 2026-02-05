package cs.system;

/** Encapsulates a method that has no parameters and does not return a value. */
@:native("System.Action")
extern class Action extends cs.system.MulticastDelegate {
	function new(func:()->Void):Void;
	function Invoke():Void;
}
