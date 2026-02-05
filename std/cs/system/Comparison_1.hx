package cs.system;

@:native("System.Comparison`1")
extern class Comparison_1<T> extends cs.system.MulticastDelegate {
	function new(func:(x:T, y:T)->Int):Void;
	function Invoke(x:T, y:T):Int;
}
