class Main {
	static function main() {
		test(std.Type.ValueType);
	}

	static function test<T>(e:Enum<T>) {
		trace(e.getName());
	}
}

