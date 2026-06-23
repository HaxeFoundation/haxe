class Main {
	static function main() {
		f();
	}

	static function f(?a:A):Void {}
}

// two resolvers whose implicit arguments reference each other cannot terminate
@:implicitArgResolver(resolve)
abstract A(String) {
	static function resolve(?b:B):A return cast "x";
}

@:implicitArgResolver(resolve)
abstract B(String) {
	static function resolve(?a:A):B return cast "x";
}
