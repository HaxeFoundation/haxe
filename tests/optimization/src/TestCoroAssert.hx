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

	// ---- outcome = {noSuspend: true} ----

	// A pre-transformed coroutine that never suspends but may throw.
	@:coroutine(transformed, outcome = {noSuspend: true})
	static function neverSuspendingInt(cont:haxe.coro.IContinuation<Int>):haxe.coro.SuspensionResult<Int> {
		return haxe.coro.SuspensionResult.withResult(42);
	}

	// Calling a no-suspend coroutine in tail position: single state (same as Sometimes).
	@:coroutine(assert = {numStates: 1})
	static function fCallNeverTail():Int {
		return neverSuspendingInt();
	}

	// Calling a no-suspend coroutine with code after it: still a single state (inline path).
	// Inner switch has Returned + Thrown cases only (no Pending).
	@:coroutine(assert = {numStates: 1, numHoisted: 0})
	static function fCallNeverWithMore():Int {
		var x = neverSuspendingInt();
		return x + 1;
	}

	// ---- outcome = {noSuspend: true, noThrow: true} ----

	// A pre-transformed coroutine that never suspends and never throws.
	@:coroutine(transformed, outcome = {noSuspend: true, noThrow: true})
	static function nosuspendNothrowInt(cont:haxe.coro.IContinuation<Int>):haxe.coro.SuspensionResult<Int> {
		return haxe.coro.SuspensionResult.withResult(42);
	}

	// Calling a no-suspend+no-throw coroutine in tail position: single state.
	@:coroutine(assert = {numStates: 1})
	static function fCallNosuspendNothrowTail():Int {
		return nosuspendNothrowInt();
	}

	// Calling a no-suspend+no-throw coroutine with code after: single state, no hoisted,
	// and no state switch at all (direct .result access).
	@:coroutine(assert = {numStates: 1, numHoisted: 0})
	static function fCallNosuspendNothrowWithMore():Int {
		var x = nosuspendNothrowInt();
		return x + 1;
	}

	// ---- outcome = {noThrow: true} ----

	// A pre-transformed coroutine that may suspend but never throws.
	@:coroutine(transformed, outcome = {noThrow: true})
	static function nothrowOnlyInt(cont:haxe.coro.IContinuation<Int>):haxe.coro.SuspensionResult<Int> {
		return haxe.coro.SuspensionResult.withResult(42);
	}

	// Calling a no-throw coroutine in tail position: single state.
	@:coroutine(assert = {numStates: 1})
	static function fCallNothrowTail():Int {
		return nothrowOnlyInt();
	}

	// Calling a no-throw coroutine with code after: two states (can still suspend),
	// but inner switch has Pending + Returned cases only (no Thrown).
	@:coroutine(assert = {numStates: 2})
	static function fCallNothrowWithMore():Int {
		var x = nothrowOnlyInt();
		return x + 1;
	}

	// ---- outcome = {noReturn: true, noThrow: true} ----

	// A pre-transformed coroutine known to always suspend (therefore never returns/throws).
	@:coroutine(transformed, outcome = {noReturn: true, noThrow: true})
	static function alwaysSuspendingInt(cont:haxe.coro.IContinuation<Int>):haxe.coro.SuspensionResult<Int> {
		return new haxe.coro.SuspensionResult(Pending);
	}

	// Calling an always-suspend coroutine in tail position: single state.
	@:coroutine(assert = {numStates: 1})
	static function fCallAlwaysTail():Int {
		return alwaysSuspendingInt();
	}

	// Calling an always-suspend coroutine with code after it: two states.
	@:coroutine(assert = {numStates: 2})
	static function fCallAlwaysWithMore():Int {
		var x = alwaysSuspendingInt();
		return x + 1;
	}

	// ---- default outcome (no special configuration) ----

	@:coroutine(transformed)
	static function sometimesSuspending(cont:haxe.coro.IContinuation<Int>):haxe.coro.SuspensionResult<Int> {
		return haxe.coro.SuspensionResult.withResult(0);
	}

	@:coroutine(assert = {numStates: 1})
	static function fCallSometimesTail():Int {
		return sometimesSuspending();
	}

	static function main() {}
}
