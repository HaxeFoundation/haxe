package yield;

import yield.Yield;

@:build(yield.YieldMacro.build())
class TestYieldWhile extends BaseCase {

	public function testWhile_basicYieldReturn() {
		assert([11, 21, 31], while_basicYieldReturn(1));
		Assert.equals('01122334', dummy);
	}

	@:yield function while_basicYieldReturn(arg:Int) {
		dummy += '0';
		var i = 1;
		while(i < 4) {
			dummy += i;
			@:yield return i * 10 + arg;
			dummy += i;
			i++;
		}
		dummy += '4';
	}


	public function testWhile_basicYieldBreak() {
		assert([10], while_basicYieldBreak());
		Assert.equals('012', dummy);
	}

	@:yield function while_basicYieldBreak() {
		dummy += '0';
		@:yield return 10;
		dummy += '1';
		var i = 2;
		while(i < 100) {
			dummy += i;
			return;
			dummy += i;
			i++;
		}
		dummy += '101';
	}


	public function testWhile_nested() {
		assert([0, 1, 10, 11], while_nested());
		Assert.equals('0[><><][><><]2', dummy);
	}

	@:yield function while_nested() {
		dummy += '0';
		var i = 0;
		while(i < 2) {
			dummy += '[';
			var j = 0;
			while(j < 2) {
				dummy += '>';
				@:yield return i * 10 + j;
				dummy += '<';
				j++;
			}
			dummy += ']';
			i++;
		}
		dummy += '2';
	}


	public function testWhile_breakContinue() {
		assert([0, -1, 2], while_breakContinue());
		Assert.equals('12356789235235670', dummy);
	}

	@:yield function while_breakContinue() {
		dummy += '1';
		var i = -1;
		while(i < 10) {
			i++;
			dummy += '2';
			while(true) {
				dummy += '3';
				break;
				dummy += '4';
			}
			dummy += '5';
			if(i == 1) continue;
			dummy += '6';
			@:yield return i;
			dummy += '7';
			if(i == 2) break;
			dummy += '8';
			@:yield return -1;
			dummy += '9';
		}
		dummy += '0';
	}
}