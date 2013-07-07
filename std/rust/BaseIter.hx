package rust;

@:native("BaseIter") extern interface BaseIter<T> {
	public function each(f:T -> Bool):Void;
	public function size_hint():Null<Int>;
}