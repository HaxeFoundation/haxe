package issues;

private class ConstructorTest {
	public inline function new(str:String, call:String->Void) {
		call(str);
	}
}

class Issue8315 {
	@:js('
		(function(str) {
			issues_Issue8315.use(str);
		})("test");
	')
	static function test() {
		new ConstructorTest("test", function(str) {
			use(str);
		});
	}

	@:pure(false) static function use(s:String) {}
}
