class Main {
	static function main() {
		f();
	}

	static function f(?c:Ctx):Void {}
}

// a resolver whose own implicit argument is its own type cannot terminate
@:implicitArgResolver(resolve)
abstract Ctx(String) {
	static function resolve(?self:Ctx):Ctx return cast "x";
}
