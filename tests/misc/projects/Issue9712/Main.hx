class Main {
	static function main() {
		var map = (['a' => 10]:Map<AbstractString, Int>);
		for (key => val in map) trace(key, val);
	}
}

abstract AbstractString(String) from String { }