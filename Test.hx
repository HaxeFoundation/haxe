@:nullSafety(Strict)
class Test {
	static final a:Array<Null<String>> = ["hello", "world"];

	static function main() {
		var event:Null<String> = null;
		while ((event = a.pop()) != null) {
			event.charAt(0); // This should NOT produce an error with our fix
		}
		trace("Test passed!");
	}
}
