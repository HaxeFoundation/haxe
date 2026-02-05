package cs.system;

/** Combines the hash code for multiple values into a single hash code. */
@:native("System.HashCode")
extern class HashCode extends cs.system.ValueType {
	@:overload(function<T1>(value1:T1):Int {})
	@:overload(function<T1, T2>(value1:T1, value2:T2):Int {})
	@:overload(function<T1, T2, T3>(value1:T1, value2:T2, value3:T3):Int {})
	@:overload(function<T1, T2, T3, T4>(value1:T1, value2:T2, value3:T3, value4:T4):Int {})
	@:overload(function<T1, T2, T3, T4, T5>(value1:T1, value2:T2, value3:T3, value4:T4, value5:T5):Int {})
	@:overload(function<T1, T2, T3, T4, T5, T6>(value1:T1, value2:T2, value3:T3, value4:T4, value5:T5, value6:T6):Int {})
	@:overload(function<T1, T2, T3, T4, T5, T6, T7>(value1:T1, value2:T2, value3:T3, value4:T4, value5:T5, value6:T6, value7:T7):Int {})
	/**
	 * Diffuses the hash code returned by the specified value.
	 * @param T1 The type of the value to add the hash code.
	 * @param value1 The value to add to the hash code.
	 * @return The hash code that represents the single value.
	 */
	static function Combine<T1, T2, T3, T4, T5, T6, T7, T8>(value1:T1, value2:T2, value3:T3, value4:T4, value5:T5, value6:T6, value7:T7, value8:T8):Int;
	@:overload(function<T>(value:T):Void {})
	/**
	 * Adds a single value to the hash code.
	 * @param T The type of the value to add to the hash code.
	 * @param value The value to add to the hash code.
	 */
	function Add<T>(value:T, comparer:cs.system.collections.generic.IEqualityComparer<T>):Void;
	/**
	 * This method is not supported and should not be called.
	 * @param obj Ignored.
	 * @return This method will always throw a .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * This method is not supported and should not be called.
	 * @return This method will always throw a .
	 */
	function GetHashCode():Int;
	/**
	 * Calculates the final hash code after consecutive  invocations.
	 * @return The calculated hash code.
	 */
	function ToHashCode():Int;
}
