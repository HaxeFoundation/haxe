typedef Thenable<T> = {
	function then<TOut>():Thenable<TOut>;
}

class Main3 {
	static function main() {
		({then: () -> null} : Thenable<String>);
	}
}