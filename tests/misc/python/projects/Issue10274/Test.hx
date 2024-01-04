@:nullSafety
class Test {
	public static function toArray<T>(item:Null<T>, times:Int):Array<Null<T>> {
		final arr:Array<Null<T>> = [];
		for (i in 0...times)
			arr.push(item);
		return arr;
	}

	static function main() {}
}
