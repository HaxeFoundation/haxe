class TestCoroAssert {
	@:coroutine static function inner():Void {}
	@:coroutine static function innerInt():Int { return 42; }

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

	// No argument used -> 0 hoisted fields.
	@:coroutine(assert = {numHoisted: 0})
	static function fNoHoisted():Void {
		inner();
		inner();
	}

	// One argument, used across states -> 1 hoisted field.
	@:coroutine(assert = {numHoisted: 1})
	static function fOneHoisted(x:Int):Int {
		inner();
		return x;
	}

	// Two arguments, both used across states -> 2 hoisted fields.
	@:coroutine(assert = {numHoisted: 2})
	static function fTwoHoisted(x:Int, y:Int):Int {
		inner();
		return x + y;
	}

	// Argument used only before the first suspension is still hoisted (all args are
	// force-hoisted; the optimisation only removes those never accessed at all).
	@:coroutine(assert = {numHoisted: 1})
	static function fArgUsedBeforeSuspension(x:Int):Void {
		var _ = x;   // read in state 0, before the first suspension
		inner();
	}

	// Unused argument (discarded wildcard-style) is not hoisted.
	@:coroutine(assert = {numHoisted: 0})
	static function fUnusedArg(x:Int):Void {
		inner();
		inner();
	}

	// Local variable used across states -> 1 hoisted field.
	@:coroutine(assert = {numHoisted: 1})
	static function fLocalHoisted():Int {
		var x = innerInt();
		inner();
		return x;
	}

	// Can combine numStates and numHoisted in a single assert.
	@:coroutine(assert = {numStates: 2, numHoisted: 1})
	static function fCombinedAssert(x:Int):Int {
		inner();
		return x;
	}

	static function main() {}
}
