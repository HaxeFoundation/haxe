package cs.system.threading;

/** Provides support for spin-based waiting. */
@:native("System.Threading.SpinWait")
extern class SpinWait extends cs.system.ValueType {
	/**
	 * Gets the number of times  has been called on this instance.
	 * @return Returns an integer that represents the number of times  has been called
	 * on this instance.
	 */
	var Count(default, never):Int;
	/**
	 * Gets whether the next call to  will yield the processor, triggering a forced
	 * context switch.
	 * @return Whether the next call to  will yield the processor, triggering a forced
	 * context switch.
	 */
	var NextSpinWillYield(default, never):Bool;
	@:overload(function(condition:cs.system.Func_1<Bool>):Void {})
	@:overload(function(condition:cs.system.Func_1<Bool>, millisecondsTimeout:Int):Bool {})
	/**
	 * Spins until the specified condition is satisfied.
	 * @param condition A delegate to be executed over and over until it returns true.
	 */
	static function SpinUntil(condition:cs.system.Func_1<Bool>, timeout:cs.system.TimeSpan):Bool;
	/** Resets the spin counter. */
	function Reset():Void;
	/** Performs a single spin. */
	function SpinOnce():Void;
}
