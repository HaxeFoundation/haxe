@:nativeGen extern class NativeArray<T> implements ArrayAccess implements BaseIter<T> {
	public function new() {}
	public function each(f:T -> Bool):Void {}
	public function size_hint():Null<Int> {return null;}
	public function capacity():Int;
	public function len():Int;
	public function filter(f:T -> Bool):NativeArray<T>;
	public function from_elem(len:Int, d:T):NativeArray<T>;
	public function grow(n:Int, initv:T):NativeArray<T>;
	public function head():T;
	public function is_empty():Bool;
}