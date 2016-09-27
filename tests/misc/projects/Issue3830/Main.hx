@:callable
@:noFollow
abstract FunctionPointer<T:haxe.Constraints.Function>(T) from T {}

class Main {
	static function main() {
		var f:FunctionPointer<String->Void> = test;
		f(|
	}

	static function test(s:String) { }
}