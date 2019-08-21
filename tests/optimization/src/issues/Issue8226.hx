package issues;

class Issue8226 {
	@:pure(false) static var testStatic:String;
	static var pureStatic:String;

	@:pure(false) var testInstance:String;
	var pureInstance:String;

	@:js('
		issues_Issue8226.testStatic;
		new issues_Issue8226().testInstance;
	')
	static function test() {
		testStatic;
		pureStatic;

		var m = new Issue8226();
		m.testInstance;
		m.pureInstance;
	}

	public function new() {}
}