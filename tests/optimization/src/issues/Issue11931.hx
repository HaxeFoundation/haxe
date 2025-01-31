package issues;

class Issue11931 {
	@:js('
		var arr = [];
		var e = arr[0];
		issues_Issue11931.use(e != null ? e : arr[0] = []);
	')
	static function test() {
		var arr = [];
		var e = arr[0] ??= [];
		use(e);
	}

	@:pure(false)
	static function use(v:Array<Int>) {}
}
