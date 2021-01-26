@:multiType(T)
abstract Vector<T>(Array<T>) {
	function new();

	public inline static function ofArray<T>(a:Array<T>):Vector<T> {
		return new Vector<T>();
	}

	@:to static function toIntVector(t:Array<Int>)
		return null;

	@:to static function toObjectVector<T>(t:Array<T>)
		return null;
}
