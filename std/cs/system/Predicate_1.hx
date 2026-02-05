package cs.system;

@:native("System.Predicate`1")
extern class Predicate_1<T> extends cs.system.MulticastDelegate {
	function new(func:(obj:T)->Bool):Void;
	function Invoke(obj:T):Bool;
}
