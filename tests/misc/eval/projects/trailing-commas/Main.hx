class Main {
	static function f(t:Type.ValueType, t2:Type.ValueType,) {}
	public static function main():Void {
		f(TNull,TNull,);
		foobar(
			1,
			2,
		);
		function blah(
			a:Int,
			b:Int,
		):Void {}
		var a = (
			a:Int,
			b:Int,
		) -> 0;
		var a:(
			a:Int,
			b:Int,
		)->Int = null;
		a = (a, b,) -> a + b;
		trace(a(2, 3,) + moduleFoobar(5, 5,));
	}

	static function foobar(
		a:Int,
		b:Int,
	):Void {}
}

enum Foo {
	Bar(
		a:Int,
		b:Int,
	);
}

function moduleFoobar(
	a:Int,
	b:Int,
):Int {
	return a + b;
}
