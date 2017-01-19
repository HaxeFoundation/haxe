package issues;

class Issue5509 {
	@:js('
		issues_Issue5509["use"]();
	')
	static function test() {
        var v = "some";
        if (v != null)
			use();
	}

	@:pure(false) static function use() { }
}