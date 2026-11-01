// `@:overload @:native(X)` makes multiple Haxe fields share a JVM method
// name. genjvm now groups by name in _hx_getField and produces a single closure
// class hosting all overloads' invoke methods.
class Buf {
	public function new() {}
	public function add(x:Dynamic):Void Sys.println('dyn:$x');
	@:overload @:native("add") public function addBool(b:Bool):Void Sys.println('bool:$b');
}

class Main2 {
	static public function main() {
		final b = new Buf();
		final dyn = Reflect.field(b, "add");
		// Reflect.callMethod boxes args, so both calls dispatch via
		// invoke(Object) -> the Dynamic overload. The point of this test
		// isn't the dispatch outcome — it's that the jar contains exactly
		// one Buf$..._add.class entry and that the _hx_getField bytecode
		// has no duplicate switch cases (checked by Check.hx).
		Reflect.callMethod(b, dyn, [true]);
		Reflect.callMethod(b, dyn, ["hello"]);
	}
}
