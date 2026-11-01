package issues;

class Extensions {
	inline static public function findMin<T>(a:Array<T>, f:T->Float) {
		var minVal = 0.0;
		for (item in a) {
			var v = f(item);
			minVal = v;
		}
		return {val: minVal};
	}

	inline static public function findMaxValue<T>(a:Array<T>, f:T->Float):Float {
		return -findMin(a, i -> -f(i)).val;
	}
}

class Issue7888 {
	@:js('
		var arr = [5,4];
		var minVal = 0.0;
		var _g = 0;
		while(_g < arr.length) minVal = -(arr[_g++] * 2);
		issues_Issue7888.use(-minVal);
	')
	static function test() {
		var arr = [5, 4];
		final v = Extensions.findMaxValue(arr, e -> e * 2);
		use(v);
	}

	@:js('
		var _g = 0;
		var _g1 = Std.random(3);
		while(_g < _g1) issues_Issue7888.use(_g++);
		issues_Issue7888.use("hello");
	')
	static function test2() {
		level1Loop((i) -> use(i));
		level1NoLoop(() -> use('hello'));
	}

	static function use(v:Any):Void {
		Std.random(0);
	}

	static inline function level1Loop(cb:Int->Void) {
		level2Loop(i -> cb(i));
	}

	static inline function level2Loop(cb:Int->Void) {
		for(i in 0...Std.random(3)) {
			cb(i);
		}
	}

	static inline function level1NoLoop(cb:Void->Void) {
		level2NoLoop(() -> cb());
	}

	static inline function level2NoLoop(cb:Void->Void) {
		cb();
	}
}
