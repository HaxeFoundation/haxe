class TestTreGeneration {
	@:js('
		if(b == null) {
			b = 10;
		}
		while(true) {
			if(Std.random(2) == 0) {
				var _gtmp = a;
				a = b + a;
				b = _gtmp;
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
		if(Std.random(2) == 0) {
			return testStaticMethod(b + a, a, s + '?');
		}
		return s == null ? a : b;
	}

	@:js('
		if(b == null) {
			b = 10;
		}
		while(true) {
			if(Std.random(2) == 0) {
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
	function testInstanceMethod(a:Int, b:Int = 10, ?s:String):Int {
		if(Std.random(2) == 0) {
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
				if(Std.random(2) == 0) {
					var _gtmp = a;
					a = b + a;
					b = _gtmp;
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
			if(Std.random(2) == 0) {
				return local(b + a, a, s + '?');
			}
			return s == null ? a : b;
		}
		local(1, 2);
	}

	@:js('
		var _g = 0;
		var _g1 = Std.random(10);
		while(_g < _g1) {
			++_g;
			if(Std.random(2) == 0) {
				return TestTreGeneration.testTailRecursionInsideLoop();
			}
		}
		return Std.random(10);
	')
	static function testTailRecursionInsideLoop():Int {
		for(i in 0...Std.random(10)) {
			if(Std.random(2) == 0) {
				return testTailRecursionInsideLoop();
			}
		}
		return Std.random(10);
	}

	@:js('
		while(true) {
			if(Std.random(2) == 0) {
				a -= 1;
				continue;
			}
			if(a < 10) {
				a += 1;
				continue;
			}
			return;
		}
	')
	static function testVoid(a:Int):Void {
		if(Std.random(2) == 0) {
			testVoid(a - 1);
			return;
		}
		if(a < 10) {
			testVoid(a + 1);
		}
	}

	@:js('
		while(true) {
			try {
				if(n <= 0) {
					throw haxe_Exception.thrown("exit");
				}
				return TestTreGeneration.testTryCancelsTre(n - 1);
			} catch( _g ) {
				if(n == 0) {
					n -= 1;
					continue;
				}
			}
			return 0;
		}
	')
	static function testTryCancelsTre(n:Int):Int {
		try {
			if(n <= 0) throw 'exit';
			return testTryCancelsTre(n - 1);
		} catch(e:Dynamic) {
			if(n == 0) {
				return testTryCancelsTre(n - 1);
			}
		}
		return 0;
	}
}