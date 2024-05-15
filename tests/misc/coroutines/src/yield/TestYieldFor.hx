package yield;

import yield.Yield;

@:build(yield.YieldMacro.build())
class TestYieldFor extends BaseCase {

	public function testFor_basicYieldReturn() {
		assert([11, 21, 31], for_basicYieldReturn(1));
		Assert.equals('01122334', dummy);
	}

	@:yield function for_basicYieldReturn(arg:Int) {
		dummy += '0';
		for(i in 1...4) {
			dummy += i;
			@:yield return i * 10 + arg;
			dummy += i;
		}
		dummy += '4';
	}

	public function testFor_basicYieldBreak() {
		assert([10], for_basicYieldBreak());
		Assert.equals('012', dummy);
	}

	@:yield function for_basicYieldBreak() {
		dummy += '0';
		@:yield return 10;
		dummy += '1';
		for(i in 2...100) {
			dummy += i;
			return;
			dummy += i;
		}
		dummy += '101';
	}

	public function testFor_nested() {
		assert([0, 1, 10, 11], for_nested());
		Assert.equals('0[><><][><><]2', dummy);
	}

	@:yield function for_nested() {
		dummy += '0';
		for(i in 0...2) {
			dummy += '[';
			for(j in 0...2) {
				dummy += '>';
				@:yield return i * 10 + j;
				dummy += '<';
			}
			dummy += ']';
		}
		dummy += '2';
	}


	public function testFor_breakContinue() {
		assert([0, -1, 2], for_breakContinue());
		Assert.equals('12356789235235670', dummy);
	}

	@:yield function for_breakContinue() {
		dummy += '1';
		for(i in 0...10) {
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