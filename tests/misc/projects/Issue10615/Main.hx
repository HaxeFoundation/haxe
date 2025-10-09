@:js.import(@star '../lib.js')
extern class Lib {
	@:native("default") static function default_():Bool;
	static var str:String;
	static function increment(i:Int):Int;
}

@:js.import(@star '../lib_default_fun.js')
extern class DefaultFun {
	@:native("default") static function default_():Bool;
}

@:js.import(@default '../lib_default_class.js')
extern class DefaultClass {
	static function def():Bool;
}

@:js.import('../multilib.js')
extern class Foo {
	static function name():String;
}
@:js.import('../multilib.js', 'Foo')
extern class Foo2 {
	static function name():String;
}
@:js.import('../multilib.js', 'Bar')
extern class Bar2 {
	static function name():String;
}

@:js.import(@default '../multilib.js')
extern class DefaultClass2 {
	function new();
	function name():Bool;
}

class Main {
	static function main() {
		eq("str", Lib.str);
		eq(2, Lib.increment(1));
		eq("default function", Lib.default_());

		eq("foo", Foo.name());
		eq("foo", Foo2.name());
		eq("bar", Bar2.name());
		final def = new DefaultClass2();
		eq("default class", def.name());

		eq("default function", DefaultFun.default_());
		eq("default static", DefaultClass.def());
	}

	static function eq(a:Any, b:Any):Void {
		if (a != b) throw '$a is not $b';
	}
}
