class Main {
	static function main() {
		final f = (_) -> {
			throw "This function shouldn't have been called!!";
		}
		("hello");

		final f = function(_) {
			throw "This function shouldn't have been called!!";
		}
		("hello");

		// named locals
		function f(_) {
			throw "This function shouldn't have been called!!";
		}
		("hello");

		final g = function f(_) {
			throw "This function shouldn't have been called!!";
		}
		("hello");
	}
}
