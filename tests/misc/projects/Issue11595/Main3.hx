class Main3 extends Base {
	public static function main() {}
	override function foo(a) "";
}

private class Base {
	function foo(a:Int):String return "";
}

private class Foo {
	public public var foo:String;
	public private var bar:String;
}

typedef Bar = {
	var bar:String;
}

typedef Baz = Bar & {
	var bar:Int;
	var baz:Int;
}
