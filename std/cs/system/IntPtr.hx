package cs.system;

/** A platform-specific type that is used to represent a pointer or a handle. */
@:native("System.IntPtr")
extern class IntPtr extends cs.system.ValueType {
	/** A read-only field that represents a pointer or handle that has been initialized to zero. */
	static var Zero(default, never):cs.system.IntPtr;
	/**
	 * Gets the size of this instance.
	 * @return The size of a pointer or handle in this process, measured in bytes. The
	 * value of this property is 4 in a 32-bit process, and 8 in a 64-bit process. You
	 * can define the process type by setting the  switch when you compile your code
	 * with the C# and Visual Basic compilers.
	 */
	static var Size(default, never):Int;
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	function new(value:cs.Pointer<Void>):Void;
	/**
	 * Adds an offset to the value of a pointer.
	 * @param pointer The pointer to add the offset to.
	 * @param offset The offset to add.
	 * @return A new pointer that reflects the addition of  to .
	 */
	static function Add(pointer:cs.system.IntPtr, offset:Int):cs.system.IntPtr;
	/**
	 * Adds an offset to the value of a pointer.
	 * @param pointer The pointer to add the offset to.
	 * @param offset The offset to add.
	 * @return A new pointer that reflects the addition of  to .
	 */
	static function op_Addition(pointer:cs.system.IntPtr, offset:Int):cs.system.IntPtr;
	/**
	 * Determines whether two specified instances of  are equal.
	 * @param value1 The first pointer or handle to compare.
	 * @param value2 The second pointer or handle to compare.
	 * @return if  equals ; otherwise, .
	 */
	static function op_Equality(value1:cs.system.IntPtr, value2:cs.system.IntPtr):Bool;
	@:overload(function(value:Int):cs.system.IntPtr {})
	@:overload(function(value:haxe.Int64):cs.system.IntPtr {})
	@:overload(function(value:cs.system.IntPtr):Int {})
	@:overload(function(value:cs.system.IntPtr):haxe.Int64 {})
	@:overload(function(value:cs.system.IntPtr):cs.Pointer<Void> {})
	/**
	 * Converts the value of a 32-bit signed integer to an .
	 * @param value A 32-bit signed integer.
	 * @return A new instance of  initialized to .
	 */
	static function op_Explicit(value:cs.Pointer<Void>):cs.system.IntPtr;
	/**
	 * Determines whether two specified instances of  are not equal.
	 * @param value1 The first pointer or handle to compare.
	 * @param value2 The second pointer or handle to compare.
	 * @return if  does not equal ; otherwise, .
	 */
	static function op_Inequality(value1:cs.system.IntPtr, value2:cs.system.IntPtr):Bool;
	/**
	 * Subtracts an offset from the value of a pointer.
	 * @param pointer The pointer to subtract the offset from.
	 * @param offset The offset to subtract.
	 * @return A new pointer that reflects the subtraction of  from .
	 */
	static function op_Subtraction(pointer:cs.system.IntPtr, offset:Int):cs.system.IntPtr;
	/**
	 * Subtracts an offset from the value of a pointer.
	 * @param pointer The pointer to subtract the offset from.
	 * @param offset The offset to subtract.
	 * @return A new pointer that reflects the subtraction of  from .
	 */
	static function Subtract(pointer:cs.system.IntPtr, offset:Int):cs.system.IntPtr;
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
	 * @param obj An object to compare with this instance or .
	 * @return if  is an instance of  and equals the value of this instance; otherwise,
	 * .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Converts the value of this instance to a 32-bit signed integer.
	 * @return A 32-bit signed integer equal to the value of this instance.
	 */
	function ToInt32():Int;
	/**
	 * Converts the value of this instance to a 64-bit signed integer.
	 * @return A 64-bit signed integer equal to the value of this instance.
	 */
	function ToInt64():haxe.Int64;
	/**
	 * Converts the value of this instance to a pointer to an unspecified type.
	 * @return A pointer to ; that is, a pointer to memory containing data of an
	 * unspecified type.
	 */
	function ToPointer():cs.Pointer<Void>;
	@:overload(function():String {})
	/**
	 * Converts the numeric value of the current  object to its equivalent string
	 * representation.
	 * @return The string representation of the value of this instance.
	 */
	function ToString(format:String):String;
}
