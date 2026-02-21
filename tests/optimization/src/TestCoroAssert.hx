class TestCoroAssert {
	@:coroutine static function inner():Void {}

	// A tail call is a single state (single-state optimization applies).
	@:coroutine(assert = {numStates: 1})
	static function fOneState():Void {
		inner();
	}

	// Two sequential suspension calls produce two states.
	@:coroutine(assert = {numStates: 2})
	static function fTwoStates():Void {
		inner();
		inner();
	}

	static function main() {}
}
