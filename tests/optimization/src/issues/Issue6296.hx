package issues;

class Issue6296 {
	@:js('
		var a1 = [];
		if(a1.push != null) {
			a1.push(1);
		}'
	)
	@:analyzer(no_local_dce)
	static function f(a, b) {
		var a = [];
		if (a.push != null) {
			a.push(1);
		}
	}
}