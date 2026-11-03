@:callable
@:coreType
abstract FunctionPointer<T:haxe.Constraints.Function> from T {}

class Main {
	static function main() {
		var f:FunctionPointer<String->Void> = test;
		f("foo");
	}

	static function test(s:String) { }
}