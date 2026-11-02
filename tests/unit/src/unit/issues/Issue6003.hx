package unit.issues;

private abstract Struct<T:{}>(String) from String to String {
	@:from static public function fromObject<T:{}>(v:T):Struct<T> {
		var nested:Dynamic = Reflect.field(v, "nested");
		return nested == null ? "leaf" : "outer(" + (Std.isOfType(nested, String) ? nested : "unconverted") + ")";
	}
}

private typedef Options = {?nested:Struct<{?field:String}>}
private typedef RequiredOptions = {nested:Struct<{?field:String}>}

class Issue6003 extends unit.Test {
	function test() {
		eq("outer(leaf)", check({nested: {field: "world"}}));
		eq("outer(leaf)", check(get()));
		eq("outer(leaf)", checkRequired({nested: {field: "world"}}));
	}

	static function check(o:Struct<Options>):String {
		return o;
	}

	static function checkRequired(o:Struct<RequiredOptions>):String {
		return o;
	}

	static function get():Options {
		return {nested: {field: "world"}};
	}
}
