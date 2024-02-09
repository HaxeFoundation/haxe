package issues;

@:generic
private class MyType<@:const T> {
    public function new() { }

	public inline function constGenericInlineWtf() {
        js.Syntax.code('console.log({0})', T);
    }
}

class Issue7113 {
	@:js('
		console.log("myfile");
	')
	static function test() {
		new MyType<"myfile">().constGenericInlineWtf();
	}
}