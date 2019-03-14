package eval.vm;

extern class Deque<T> {
	function new():Void;
	function add(i:T):Void;
	function pop(block:Bool):Null<T>;
	function push(i:T):Void;
}