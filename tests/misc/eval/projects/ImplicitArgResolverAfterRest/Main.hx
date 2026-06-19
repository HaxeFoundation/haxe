class Main {
	static function main() {}

	// a resolver-typed optional cannot follow a rest argument
	// (only a trailing PosInfos is reordered before the rest)
	static function f(...rest:Int, ?c:Ctx):Void {}
}

@:implicitArgResolver(resolve)
abstract Ctx(String) {
	static function resolve():Ctx return cast "x";
}
