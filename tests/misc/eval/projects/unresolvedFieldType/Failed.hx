class Failed {
	// its typing reports an error: not also reported as unresolved
	var cache = {this; null;};

	function new() {}

	static function main() {
		new Failed().cache = "a";
	}
}
