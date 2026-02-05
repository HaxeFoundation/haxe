package cs.system;

@:native("System.Predicate")
extern class Predicate<T> extends cs.system.MulticastDelegate {
	function new(func:(obj:T)->Bool):Void;
	function Invoke(obj:T):Bool;
}
