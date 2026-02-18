@:nullSafety(Strict)
class Test {
	static final a:Array<Null<String>> = [];

	static function main() {
		var event:Null<String> = null;
		while ((event = a.pop()) != null) {
			event.charAt(0);
		}
	}
}
