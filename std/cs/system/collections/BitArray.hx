package cs.system.collections;

/** Manages a compact array of bit values, which are represented as Booleans, where  indicates that the bit is on (1) and  indicates the bit is off (0). */
@:native("System.Collections.BitArray")
extern class BitArray {
	/**
	 * Gets the number of elements contained in the .
	 * @return The number of elements contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether the  is read-only.
	 * @return This property is always .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return This property is always .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets or sets the number of elements in the .
	 * @return The number of elements in the .
	 */
	var Length(default, default):Int;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	@:native("get_Item")
	function get_Item(index0:Int):Bool;
	@:native("set_Item")
	function set_Item(index0:Int, value:Bool):Void;
	@:overload(function(values:cs.NativeArray<Bool>):Void {})
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(bits:cs.system.collections.BitArray):Void {})
	@:overload(function(length:Int):Void {})
	@:overload(function(values:cs.NativeArray<Int>):Void {})
	function new(length:Int, defaultValue:Bool):Void;
	/**
	 * Performs the bitwise AND operation between the elements of the current  object
	 * and the corresponding elements in the specified array. The current  object will
	 * be modified to store the result of the bitwise AND operation.
	 * @param value The array with which to perform the bitwise AND operation.
	 * @return An array containing the result of the bitwise AND operation, which is a
	 * reference to the current  object.
	 */
	function And(value:cs.system.collections.BitArray):cs.system.collections.BitArray;
	/**
	 * Creates a shallow copy of the .
	 * @return A shallow copy of the .
	 */
	function Clone():Dynamic;
	/**
	 * Copies the entire  to a compatible one-dimensional , starting at the specified
	 * index of the target array.
	 * @param array The one-dimensional  that is the destination of the elements copied
	 * from . The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Gets the value of the bit at a specific position in the .
	 * @param index The zero-based index of the value to get.
	 * @return The value of the bit at position .
	 */
	function Get(index:Int):Bool;
	/**
	 * Returns an enumerator that iterates through the .
	 * @return An  for the entire .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/** @param count  */
	function LeftShift(count:Int):cs.system.collections.BitArray;
	/**
	 * Inverts all the bit values in the current , so that elements set to  are changed
	 * to , and elements set to  are changed to .
	 * @return The current instance with inverted bit values.
	 */
	function Not():cs.system.collections.BitArray;
	/**
	 * Performs the bitwise OR operation between the elements of the current  object
	 * and the corresponding elements in the specified array. The current  object will
	 * be modified to store the result of the bitwise OR operation.
	 * @param value The array with which to perform the bitwise OR operation.
	 * @return An array containing the result of the bitwise OR operation, which is a
	 * reference to the current  object.
	 */
	function Or(value:cs.system.collections.BitArray):cs.system.collections.BitArray;
	/** @param count  */
	function RightShift(count:Int):cs.system.collections.BitArray;
	/**
	 * Sets the bit at a specific position in the  to the specified value.
	 * @param index The zero-based index of the bit to set.
	 * @param value The Boolean value to assign to the bit.
	 */
	function Set(index:Int, value:Bool):Void;
	/**
	 * Sets all bits in the  to the specified value.
	 * @param value The Boolean value to assign to all bits.
	 */
	function SetAll(value:Bool):Void;
	/**
	 * Performs the bitwise exclusive OR operation between the elements of the current 
	 * object against the corresponding elements in the specified array. The current 
	 * object will be modified to store the result of the bitwise exclusive OR
	 * operation.
	 * @param value The array with which to perform the bitwise exclusive OR operation.
	 * @return An array containing the result of the bitwise exclusive OR operation,
	 * which is a reference to the current  object.
	 */
	function Xor(value:cs.system.collections.BitArray):cs.system.collections.BitArray;
}
