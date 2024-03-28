package yield;

import yield.Yield;

private enum Example {
	One;
	Two(v:Int);
	Three(v:String);
	Four;
}

@:build(yield.YieldMacro.build())
class TestYieldSwitch extends BaseCase {

	public function testSwitch() {
		assert([10, 30], basicSwitch(One));
		Assert.equals('1230-', dummy);
		assert([20, 30], basicSwitch(Two(20)));
		Assert.equals('1450-', dummy);
		assert([5, 30], basicSwitch(Three('hello')));
		Assert.equals('1670-', dummy);
		assert([30], basicSwitch(Three('h')));
		Assert.equals('1h0-', dummy);
		assert([], basicSwitch(Four));
		Assert.equals('18', dummy);
	}

	@:yield function basicSwitch(arg) {
		dummy += '1';
		switch(arg) {
			case One:
				dummy += '2';
				@:yield return 10;
				dummy += '3';
			case Two(v):
				dummy += '4';
				@:yield return v;
				dummy += '5';
			case Three(v) if(v.length > 1):
				dummy += '6';
				@:yield return v.length;
				dummy += '7';
			case Three(v):
				dummy += v;
			default:
				dummy += '8';
				return;
				dummy += '9';
		}
		dummy += '0';
		@:yield return 30;
		dummy += '-';
	}

	#if broken
	public function testSwitch_withoutYield() {
		assert([30], switch_withoutYield(One));
		assert([30], switch_withoutYield(Two(10)));
		assert([30], switch_withoutYield(Three('hello')));
		assert([30], switch_withoutYield(Four));
	}

	@:yield function switch_withoutYield(arg) {
		var state = __ctx__.state;
		switch(arg) {
			case One: Assert.equals(state, __ctx__.state);
			case Two(v): Assert.equals(state, __ctx__.state);
			case Three(v): Assert.equals(state, __ctx__.state);
			case _: Assert.equals(state, __ctx__.state);
		}
		Assert.equals(state, __ctx__.state);
		@:yield return 30;
	}
	#end

	public function testSwitch_multipleSwitch() {
		assert([20, 30, 40], switch_multipleSwitch(One));
		assert([10, 20, 40], switch_multipleSwitch(Two(999)));
	}

	@:yield function switch_multipleSwitch(arg) {
		switch(arg) {
			case Two(_): @:yield return 10;
			case _:
		}
		@:yield return 20;
		switch(arg) {
			case One: @:yield return 30;
			case _:
		}
		@:yield return 40;
	}

	public function testNoYieldSwitchAsArgument() {
		assert([10], noYieldSwitchAsArgument(10));
	}

	@:yield function noYieldSwitchAsArgument(arg:Int) {
		var fn = function(v:Int) return v;
		var result = fn(switch(arg) {
			case _: arg;
		});
		@:yield return result;
	}
}