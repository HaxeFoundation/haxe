package cs.system;

@:native("System.Comparison")
extern class Comparison<T> extends cs.system.MulticastDelegate {
	function new(func:(x:T, y:T)->Int):Void;
	function Invoke(x:T, y:T):Int;
}
