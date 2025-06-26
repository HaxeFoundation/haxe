package issues;

class Issue6302 {
	@:js('a = b && c;')
	@:analyzer(no_local_dce)
	static function f1(a, b, c) {
		a = b && c;
	}

	@:js('if(!b) {a = false;}')
	@:analyzer(no_local_dce)
	static function f2(a, b) {
		a = b && a;
	}
}