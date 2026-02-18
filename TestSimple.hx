@:nullSafety(Strict)
class TestSimple {
	static function main() {
		var a:Array<Null<String>> = [];
		var event:Null<String> = null;
		
		// First, test the simple case that should work
		event = a.pop();
		if (event != null) {
			event.charAt(0); // This should work
		}
		
		// Now test the assignment in condition
		if ((event = a.pop()) != null) {
			event.charAt(0); // This should also work with our fix
		}
	}
}
