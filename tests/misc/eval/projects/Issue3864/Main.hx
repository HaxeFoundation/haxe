class Main {
	public static function main() {
		var t:Base<String> = new Test();
		trace(t.getValue());
		var t:Base<Int> = new Test();
		trace(t.getValue());

		var t:Base = new Test();
		trace(t.getValue());
	}
}

@:genericBuild(Macro.apply())
class Test {}