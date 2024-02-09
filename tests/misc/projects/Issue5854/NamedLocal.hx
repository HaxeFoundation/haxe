class NamedLocal {
	static function main() {
		// shouldn't be parsed as a call
		function test(str) {
			trace(str);
			throw "This function shouldn't have been called!!";
		}
		("hello");
	}
}
