package rust;
extern class NativeArray<T> implements ArrayAccess<T> implements BaseIter<T> {
	public function each(f:T -> Bool):Void {}
	public function size_hint():Null<Int> {return null;}
	public function capacity():Int;
	public function add(o:NativeArray<T>):NativeArray<T>;
	public function len():Int;
	public function filter(f:T -> Bool):NativeArray<T>;
	public static function from_elem<T>(len:Int, d:T):NativeArray<T>;
	public function grow(n:Int, initv:T):NativeArray<T>;
	public function head():T;
	public function is_empty():Bool;
	public function copy():NativeArray<T>;
}