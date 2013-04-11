@:nativeGen extern class NativeArray<T> implements ArrayAccess implements BaseIter<T> {
	public function new() {}
	public function each(f:T -> Bool):Void {}
	public function size_hint():Null<Int> {return null;}
}