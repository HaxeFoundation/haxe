package cs.system;

/** Provides static methods for creating value tuples. */
@:native("System.ValueTuple")
extern class ValueTuple extends cs.system.ValueType {
	@:overload(function():cs.system.ValueTuple {})
	@:overload(function<T1>(item1:T1):cs.system.ValueTuple_1<T1> {})
	@:overload(function<T1, T2>(item1:T1, item2:T2):cs.system.ValueTuple_2<T1, T2> {})
	@:overload(function<T1, T2, T3>(item1:T1, item2:T2, item3:T3):cs.system.ValueTuple_3<T1, T2, T3> {})
	@:overload(function<T1, T2, T3, T4>(item1:T1, item2:T2, item3:T3, item4:T4):cs.system.ValueTuple_4<T1, T2, T3, T4> {})
	@:overload(function<T1, T2, T3, T4, T5>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5):cs.system.ValueTuple_5<T1, T2, T3, T4, T5> {})
	@:overload(function<T1, T2, T3, T4, T5, T6>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6):cs.system.ValueTuple_6<T1, T2, T3, T4, T5, T6> {})
	@:overload(function<T1, T2, T3, T4, T5, T6, T7>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6, item7:T7):cs.system.ValueTuple_7<T1, T2, T3, T4, T5, T6, T7> {})
	/**
	 * Creates a new value tuple with zero components.
	 * @return A new value tuple with no components.
	 */
	static function Create<T1, T2, T3, T4, T5, T6, T7, T8>(item1:T1, item2:T2, item3:T3, item4:T4, item5:T5, item6:T6, item7:T7, item8:T8):cs.system.ValueTuple_8<T1, T2, T3, T4, T5, T6, T7, cs.system.ValueTuple_1<T8>>;
	/**
	 * Compares the current  instance to a specified  instance.
	 * @param other The object to compare with the current instance.
	 * @return This method always returns 0.
	 */
	function CompareTo(other:cs.system.ValueTuple):Int;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether the current  instance is equal to a
	 * specified object.
	 * @param obj The object to compare to the current instance.
	 * @return if  is a  instance; otherwise, .
	 */
	function Equals(other:cs.system.ValueTuple):Bool;
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
