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

	static function call() {
		return "";
	}

	@:pure(false)
	static function use(v:String) {}
}