package yield;

import yield.Yield;

@:build(yield.YieldMacro.build())
class TestYieldClosure extends BaseCase {
	// @:yield function closure(arg) {
	// 	var fn = @:yield function(arg2) {
	// 	}
	// 	@:yield function another(arg2) {
	// 		trace({arg2:arg2});
	// 	}
	// }

	var anchor:Dynamic;

	public function testClosure() {
		assert([20, 40, 60, 80, 20, 40, 60, 80, 100], closure(2));
		Assert.equals('1234512345', dummy);
	}

	@:yield function closure(arg) {
		var a:Dynamic = arg;
		anchor = a;
		var fn = @:yield function(arg2) {
			var b:Dynamic = arg;
			anchor = b;
			dummy += '1';
			@:yield return arg * 10;
			dummy += '2';
			@:yield return cast a * 20; // TODO: I had to insert these casts because this was errorring with Float should be Int
			dummy += '3';
			@:yield return cast b * 30;
			dummy += '4';
			@:yield return arg2 * 40;
			dummy += '5';
		}
		for(i in fn(a)) {
			@:yield return i;
		}
		@:yield function another(arg2) {
			var b:Dynamic = arg;
			anchor = b;
			dummy += '1';
			@:yield return arg * 10;
			dummy += '2';
			@:yield return cast a * 20;
			dummy += '3';
			@:yield return cast b * 30;
			dummy += '4';
			@:yield return arg2 * 40;
			dummy += '5';
			for(i in (@:yield function() @:yield return arg2 * 50)()) {
				@:yield return i;
			}
		}
		for(i in another(a)) {
			@:yield return i;
		}
	}


	public function testClosure_nested() {
		assert([100], closure_nested(10));
	}

	@:yield function closure_nested(arg) {
		@:yield function another(arg2) {
			var fn = @:yield function() @:yield return arg2 * 10;
			for(i in fn()) @:yield return i;
		}
		for(i in another(arg)) {
			@:yield return i;
		}
	}


	public function testClosure_withoutYield() {
		assert([0, 10], closure_withoutYield(1));
	}

	@:yield function closure_withoutYield(arg:Int) {
		var fn = function() return arg * 10;
		for(i in 0...2) {
			@:yield return fn() * i;
		}
	}
}