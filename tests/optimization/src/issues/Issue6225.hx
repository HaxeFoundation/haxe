package issues;

import TestJs.use;

class Issue6225 {
	@:js('
		var b = issues_Issue6225.getB();
		issues_Issue6225.a.set_p(0 > b ? 0 : b);
	')
	@:analyzer(no_optimize)
	static function test() {
		a.p = max(0, getB());
	}

	@:js('
		var b = issues_Issue6225.getB();
		issues_Issue6225.a.set_p(0 > b ? 0 : b);
	')
	static function test2() {
		a.p = max(0, getB());
	}

    static inline function max(a:Int, b:Int):Int return a > b ? a : b;
    static function getB() return 1;
    static var a:I;
}

interface I {
    var p(get, set):Int;
}