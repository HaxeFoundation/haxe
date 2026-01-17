typedef Struct = {
	final foo:Null<Int>;
}

typedef Struct2 = {
	final foo:Int;
}

@:nullSafety
class Main {
	static var callback:(i:Int) -> Void = cast null;
	static var nullableCallback:(i:Null<Int>) -> Void = cast null;

	static var callback2:(v:Array<String>) -> Void = cast null;
	static var nullableCallback2:(v:Null<Array<String>>) -> Void = cast null;

	static var callback3:() -> Null<Struct> = cast null;
	static var nullableCallback3:() -> Struct = cast null;

	static function main() {
		var a:{?a:{b:Null<Int>}} = cast null;
		var b:{?a:{b:Int}} = cast null;
		b = a;
		final s:Struct = cast null;
		addStruct2(s);

		nullableCallback = callback;
		// tests that short type is Null<Array<...>>, not full
		nullableCallback2 = callback2;

		nullableCallback3 = callback3;
	}

	static function addStruct2(s:Struct2) {}
}
