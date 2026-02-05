package cs.system;

/** Provides static methods for creating value tuples. */
@:native("System.ValueTuple`7")
extern class ValueTuple_7<T1, T2, T3, T4, T5, T6, T7> extends cs.system.ValueType {
	var Item1:T1;
	var Item2:T2;
	var Item3:T3;
	var Item4:T4;
	var Item5:T5;
	var Item6:T6;
	var Item7:T7;
	function new(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6, item7:T7):Void;
	/**
	 * Compares the current  instance to a specified  instance.
	 * @param other The object to compare with the current instance.
	 * @return This method always returns 0.
	 */
	function CompareTo(other:cs.system.ValueTuple_7<T1, T2, T3, T4, T5, T6, T7>):Int;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether the current  instance is equal to a
	 * specified object.
	 * @param obj The object to compare to the current instance.
	 * @return if  is a  instance; otherwise, .
	 */
	function Equals(other:cs.system.ValueTuple_7<T1, T2, T3, T4, T5, T6, T7>):Bool;
	/**
	 * Returns the hash code for the current  instance.
	 * @return This method always return 0.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of this  instance.
	 * @return This method always returns "()".
	 */
	function ToString():String;
}
