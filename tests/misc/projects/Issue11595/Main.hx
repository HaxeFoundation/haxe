class Main {
	static function main() {
		foo();
	}

	static macro function foo() {
		return macro {
			var a:String = 42;
		};
	}
}
