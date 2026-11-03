class Main {
	static function main() {
		inline test();
	}

	static function test():String {
		switch Std.random(10) {
			case 0: return 'hello';
			case _:
		}
		return 'world';
	}
}