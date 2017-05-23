package issues;

class Issue6302 {
	@:js('a = b && a;')
	@:analyzer(no_local_dce)
	static function f(a, b) {
		a = b && a;
	}
}