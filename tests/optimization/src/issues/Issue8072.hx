package issues;

class Issue8072 {
	@:js('
		var _g = 0;
		var _g1 = [1,2];
		while(_g < _g1.length) issues_Issue8072.use(_g1[_g++]);
	')
	static function test() {
		var one:One = [1, 2];
		for (v in one) use(v);
	}

	@:pure(false) static function use(v:Int) {}
}

@:forward
abstract One(Array<Int>) from Array<Int> {}