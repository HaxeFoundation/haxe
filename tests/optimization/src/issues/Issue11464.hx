package issues;

class Issue11464 {
	@:js('
		var name = issues_Issue11464.call();
		issues_Issue11464.use(name != null ? name : "default");
	')
	static function test() {
		final name = call() ?? "default";
		use(name);
	}

	@:js('
		var tmp = issues_Issue11464.foo;
		var tmp1 = issues_Issue11464.foo;
		issues_Issue11464.v = { "" : tmp != null ? tmp : "empty", "user-agent" : tmp1 != null ? tmp1 : "Mozilla"};
	')
	static function testStructureField() {
		v = {
			"": foo ?? "empty",
			"user-agent": foo ?? "Mozilla",
		};
	}

	static var foo = "default";
	static var v:Any;

	static function call() {
		return "";
	}

	@:pure(false)
	static function use(v:String) {}
}
