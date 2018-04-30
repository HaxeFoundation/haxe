package issues;

class Issue5582 {
	@:js('
		issues_Issue5582.set_i(issues_Issue5582.i + 1);
	')
	static function test() {
        i += 1;
	}

    static var i(default, set):Int = 0;

    static function set_i(i:Int):Int {
        return Issue5582.i = i;
    }
}