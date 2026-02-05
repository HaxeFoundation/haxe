package cs.system;

/** A platform-specific type that is used to represent a pointer or a handle. */
@:native("System.UIntPtr")
extern class UIntPtr extends cs.system.ValueType {
	/** A read-only field that represents a pointer or handle that has been initialized to zero. */
	static var Zero(default, never):cs.system.UIntPtr;
	/**
	 * Gets the size of this instance.
	 * @return The size of a pointer or handle on this platform, measured in bytes. The
	 * value of this property is 4 on a 32-bit platform, and 8 on a 64-bit platform.
	 */
	static var Size(default, never):Int;
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	function new(value:cs.Pointer<Void>):Void;
	/**
	 * Adds an offset to the value of an unsigned pointer.
	 * @param pointer The unsigned pointer to add the offset to.
	 * @param offset The offset to add.
	 * @return A new unsigned pointer that reflects the addition of  to .
	 */
	static function Add(pointer:cs.system.UIntPtr, offset:Int):cs.system.UIntPtr;
	/**
	 * Adds an offset to the value of an unsigned pointer.
	 * @param pointer The unsigned pointer to add the offset to.
	 * @param offset The offset to add.
	 * @return A new unsigned pointer that reflects the addition of  to .
	 */
	static function op_Addition(pointer:cs.system.UIntPtr, offset:Int):cs.system.UIntPtr;
	/**
	 * Determines whether two specified instances of  are equal.
	 * @param value1 The first pointer or handle to compare.
	 * @param value2 The second pointer or handle to compare.
	 * @return if  equals ; otherwise, .
	 */
	static function op_Equality(value1:cs.system.UIntPtr, value2:cs.system.UIntPtr):Bool;
	@:overload(function(value:cs.UInt):cs.system.UIntPtr {})
	@:overload(function(value:cs.UInt64):cs.system.UIntPtr {})
	@:overload(function(value:cs.system.UIntPtr):cs.UInt {})
	@:overload(function(value:cs.system.UIntPtr):cs.UInt64 {})
	@:overload(function(value:cs.system.UIntPtr):cs.Pointer<Void> {})
	/**
	 * Converts the value of a 32-bit unsigned integer to an .
	 * @param value A 32-bit unsigned integer.
	 * @return A new instance of  initialized to .
	 */
	static function op_Explicit(value:cs.Pointer<Void>):cs.system.UIntPtr;
	/**
	 * Determines whether two specified instances of  are not equal.
	 * @param value1 The first pointer or handle to compare.
	 * @param value2 The second pointer or handle to compare.
	 * @return if  does not equal ; otherwise, .
	 */
	static function op_Inequality(value1:cs.system.UIntPtr, value2:cs.system.UIntPtr):Bool;
	/**
	 * Subtracts an offset from the value of an unsigned pointer.
	 * @param pointer The unsigned pointer to subtract the offset from.
	 * @param offset The offset to subtract.
	 * @return A new unsigned pointer that reflects the subtraction of  from .
	 */
	static function op_Subtraction(pointer:cs.system.UIntPtr, offset:Int):cs.system.UIntPtr;
	/**
	 * Subtracts an offset from the value of an unsigned pointer.
	 * @param pointer The unsigned pointer to subtract the offset from.
	 * @param offset The offset to subtract.
	 * @return A new unsigned pointer that reflects the subtraction of  from .
	 */
	static function Subtract(pointer:cs.system.UIntPtr, offset:Int):cs.system.UIntPtr;
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
	 * Converts the value of this instance to a pointer to an unspecified type.
	 * @return A pointer to ; that is, a pointer to memory containing data of an
	 * unspecified type.
	 */
	function ToPointer():cs.Pointer<Void>;
	/**
	 * Converts the numeric value of this instance to its equivalent string
	 * representation.
	 * @return The string representation of the value of this instance.
	 */
	function ToString():String;
	/**
	 * Converts the value of this instance to a 32-bit unsigned integer.
	 * @return A 32-bit unsigned integer equal to the value of this instance.
	 */
	function ToUInt32():cs.UInt;
	/**
	 * Converts the value of this instance to a 64-bit unsigned integer.
	 * @return A 64-bit unsigned integer equal to the value of this instance.
	 */
	function ToUInt64():cs.UInt64;
}
