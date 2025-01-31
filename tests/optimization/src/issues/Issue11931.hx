package issues;

class Issue11931 {
	@:js('
		var arr = [];
		var _g = 0;
		var _g1 = issues_Issue11931.test_i;
		while(_g < _g1) {
			var x = _g++;
			var e = arr[x];
			issues_Issue11931.use(e != null ? e : arr[x] = []);
		}
	')
	static function test() {
		static var i = 0;
		var arr:Array<Array<Int>> = [];
		for (x in 0...i) {
			var e = arr[x] ??= [];
			use(e);
		}
	}

	@:pure(false)
	static function use(v:Array<Int>) {}
}
