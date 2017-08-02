package issues;

import TestJs.use;

enum E {
    Flags( v : String );
}

class Issue6409 {
	@:js('
		var issues_E1 = null;
		var f = issues_E.Flags("");
		TestJs["use"](f);
	')
	@:analyzer(ignore)
    static function test() {
        var issues_E = null;
        var f : E = Flags("");
        use(f);
    }
}