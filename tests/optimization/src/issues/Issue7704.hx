package issues;

class Issue7704 {
	@:js('
		var i = 0;
		var a = [];
		a[i] = i;
		++i;
		a[i] = i;
		issues_Issue7704.use(a);
	')
	@:analyzer(no_optimize)
	static function test() {
		var i = 0;
		var a = [];

		a[i] = i;
		i++;
		a[i] = i;

		use(a);
	}

	@:pure(false) static function use(a) { }
}