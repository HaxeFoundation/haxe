class Main {
	static function main() {
		test('hello');
	}

	overload extern static inline function test(s:String) {}
	overload extern static inline function test(s:String, ...r:String) {}
}