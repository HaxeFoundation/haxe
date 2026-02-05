package cs.system;

/** Represents a pseudo-random number generator, which is a device that produces a sequence of numbers that meet certain statistical requirements for randomness. */
@:native("System.Random")
extern class Random {
	@:overload(function():Void {})
	function new(Seed:Int):Void;
	@:overload(function():Int {})
	@:overload(function(maxValue:Int):Int {})
	/**
	 * Returns a non-negative random integer.
	 * @return A 32-bit signed integer that is greater than or equal to 0 and less than
	 * .
	 */
	function Next(minValue:Int, maxValue:Int):Int;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Void {})
	/**
	 * Fills the elements of a specified array of bytes with random numbers.
	 * @param buffer An array of bytes to contain random numbers.
	 */
	function NextBytes(buffer:cs.system.Span<cs.UInt8>):Void;
	/**
	 * Returns a random floating-point number that is greater than or equal to 0.0, and
	 * less than 1.0.
	 * @return A double-precision floating point number that is greater than or equal
	 * to 0.0, and less than 1.0.
	 */
	function NextDouble():Float;
}
