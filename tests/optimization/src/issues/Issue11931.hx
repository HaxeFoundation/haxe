package issues;

class Issue11931 {
	@:js('
		var arr = [];
		var tmp = arr[0];
		issues_Issue11931.use(tmp == null ? arr[0] = [] : tmp);
	')
	static function test() {
		var arr = [];
		var e = arr[0] ??= [];
		use(e);
	}

	@:pure(false)
	static function use(v:Array<Int>) {}
}
