package yield;

import yield.Yield;

@:build(yield.YieldMacro.build())
class TestYieldBasic extends BaseCase {
	public function testBasicYieldReturn() {
		assert([10, 20], basicYieldReturn());
		Assert.equals('123', dummy);
	}

	@:yield function basicYieldReturn():Iterator<Int> {
		dummy += '1';
		@:yield return 10;
		dummy += '2';
		@:yield return 20;
		dummy += '3';
	}

	#if broken

	public function testBasicYieldReturn_multipleIterations() {
		var generator = basicYieldReturn();
		assert([10, 20], generator);
		Assert.equals('123', dummy);
		assert([10, 20], generator);
		Assert.equals('123', dummy);
	}

	#end

	public function testBasicYieldBreak() {
		assert([10], basicYieldBreak());
		Assert.equals('12', dummy);
	}

	@:yield function basicYieldBreak() {
		dummy += '1';
		@:yield return 10;
		dummy += '2';
		return;
		dummy += '3';
		@:yield return 20;
		dummy += '4';
	}

	public function testLocalVars() {
		assert([10, 25, 40, 19, 30], localVars(10, 20, 30));
	}

	@:yield function localVars(a:Int, b:Int, a1:Int) {
		var q = b;
		@:yield return a;
		var a = 5;
		@:yield return a + q;
		var q = q * 2;
		@:yield return q;
		for (a in 1...2) {
			q = a * 10;
		}
		for (c in 1...2) {
			q += 5;
		}
		for (i in 0...2) {
			for (j in 0...2) {
				q += i + j;
			}
		}
		@:yield return q;
		@:yield return a1;
	}

	public function testLocalVars_sameVarNameInTwoChildScopes() {
		assert([10], localVars_sameVarNameInTwoChildScopes(true));
		assert([20], localVars_sameVarNameInTwoChildScopes(false));
	}

	@:yield function localVars_sameVarNameInTwoChildScopes(condition:Bool) {
		if (condition) {
			var v = 10;
			@:yield return v;
		} else {
			var v = 'ab';
			@:yield return v.length * 10;
		}
	}

	public function testLocalFunction() {
		assert([10, 20, 30], localFunction());
	}

	@:yield function localFunction() {
		inline function local1()
			return 20;
		function local2() {
			return 30;
		}
		@:yield return 10;
		@:yield return local1();
		var value = local2();
		@:yield return value;
	}

	public function testInheritance() {
		var result = [for (it in descendantsOfParent()) it];
		Assert.equals(2, result.length);
	}

	@:yield function descendantsOfParent():Iterator<Parent> {
		@:yield return new Child1();
		@:yield return new Child2();
	}
}

private class Parent {
	public function new() {}
}

private class Child1 extends Parent {}
private class Child2 extends Parent {}

function main() {
	utest.UTest.run([
		new TestYieldBasic()
	]);
}