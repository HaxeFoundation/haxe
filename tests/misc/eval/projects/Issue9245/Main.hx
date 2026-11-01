class Main {
	static function main() {
		var foo = haxe.Json.parse("{}");
		for (key => value in foo) {}
	}
}