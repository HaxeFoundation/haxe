class UnnamedLValue {
	static function main() {
		function(str) {
			trace(str);
		}
		("hello");

		(str) -> {
			trace(str);
		}
		("hello");
	}
}
