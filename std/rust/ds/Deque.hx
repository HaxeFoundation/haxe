package rust.ds;
@:native("std::deque::Deque") extern class Deque<T> implements BaseIter<T> {
	public function len():Int;
	public function isEmpty():Bool;
	public function clear():Void;
	public function new():Void;
	public function peek_front():T;
	public function peek_back():T;
	public function get(i:Int):T;
	public function each(T -> Bool):T;
	public function pop_front():T;
	public function pop_back():T;
	public function add_front(t:T):Void;
	public function add_back(t:T):Void;
}