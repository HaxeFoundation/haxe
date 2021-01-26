package issues;

class Issue5646 {
	static var mode = true;

	@:js('
		if(issues_Issue5646.mode) {
			issues_Issue5646.use("a");
		} else {
			issues_Issue5646.use("b");
		}
	')
	static function test1() {
        switch (mode) {
            case true: use("a");
            case false: use("b");
        }
	}

	@:js('
		if(issues_Issue5646.mode) {
			issues_Issue5646.use("a");
		} else {
			issues_Issue5646.use("b");
		}
	')
	static function test2() {
        switch (mode) {
			case false: use("b");
            case true: use("a");
        }
	}

	@:pure(false) static function use(a) { }
}