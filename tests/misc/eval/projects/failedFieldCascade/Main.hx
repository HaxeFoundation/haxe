class Main {
	// fails: its type is not determined
	static function make() return new UnknownClass();

	// uses the failed field: no cascading "not fully inferred" error
	static function use() return make().foo;

	static function main() {
		trace(use());
	}
}
