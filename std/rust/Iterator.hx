package rust;

@:native("Iterator") extern interface Iterator<T> {
	public function next():Null<T>;
	public function size_hint():Null<Int>;
}