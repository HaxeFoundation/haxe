class TestTre {
	@:js('
		if(b == null) {
			b = 10;
		}
		while(true) {
			if(Std.random(1) == 0) {
				var _gtmp1 = a;
				a = b + a;
				b = _gtmp1;
				s += "?";
				continue;
			}
			if(s == null) {
				return a;
			} else {
				return b;
			}
		}
	')
	static function testStaticMethod(a:Int, b:Int = 10, ?s:String):Int {
		if(Std.random(1) == 0) {
			return testStaticMethod(b + a, a, s + '?');
		}
		return s == null ? a : b;
	}

	@:js('
		if(b == null) {
			b = 10;
		}
		while(true) {
			if(Std.random(1) == 0) {
				var _gtmp1 = a;
				a = b + a;
				b = _gtmp1;
				s += "?";
				continue;
			}
			if(s == null) {
				return a;
			} else {
				return b;
			}
		}
	')
	static function testInstanceMethod(a:Int, b:Int = 10, ?s:String):Int {
		if(Std.random(1) == 0) {
			return testInstanceMethod(b + a, a, s + '?');
		}
		return s == null ? a : b;
	}

	@:js('
		var local = null;
		local = function(a,b,s) {
			if(b == null) {
				b = 10;
			}
			while(true) {
				if(Std.random(1) == 0) {
					var _gtmp1 = a;
					a = b + a;
					b = _gtmp1;
					s += "?";
					continue;
				}
				if(s == null) {
					return a;
				} else {
					return b;
				}
			}
		};
		local(1,2);
	')
	static function testLocalNamedFunction() {
		function local(a:Int, b:Int = 10, ?s:String):Int {
			if(Std.random(1) == 0) {
				return local(b + a, a, s + '?');
			}
			return s == null ? a : b;
		}
		local(1, 2);
	}
}