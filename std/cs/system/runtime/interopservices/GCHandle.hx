package cs.system.runtime.interopservices;

/** Provides a way to access a managed object from unmanaged memory. */
@:native("System.Runtime.InteropServices.GCHandle")
extern class GCHandle extends cs.system.ValueType {
	/**
	 * Gets a value indicating whether the handle is allocated.
	 * @return if the handle is allocated; otherwise, .
	 */
	var IsAllocated(default, never):Bool;
	/**
	 * Gets or sets the object this handle represents.
	 * @return The object this handle represents.
	 */
	var Target(default, default):Dynamic;
	@:overload(function(value:Dynamic):cs.system.runtime.interopservices.GCHandle {})
	/**
	 * Allocates a  handle for the specified object.
	 * @param value The object that uses the .
	 * @return A new  that protects the object from garbage collection. This  must be
	 * released with  when it is no longer needed.
	 */
	static function Alloc(value:Dynamic, type:cs.system.runtime.interopservices.GCHandleType):cs.system.runtime.interopservices.GCHandle;
	/**
	 * Returns a new  object created from a handle to a managed object.
	 * @param value An  handle to a managed object to create a  object from.
	 * @return A new  object that corresponds to the value parameter.
	 */
	static function FromIntPtr(value:cs.system.IntPtr):cs.system.runtime.interopservices.GCHandle;
	/**
	 * Returns a value indicating whether two  objects are equal.
	 * @param a A  object to compare with the  parameter.
	 * @param b A  object to compare with the  parameter.
	 * @return if the  and  parameters are equal; otherwise, .
	 */
	static function op_Equality(a:cs.system.runtime.interopservices.GCHandle, b:cs.system.runtime.interopservices.GCHandle):Bool;
	@:overload(function(value:cs.system.IntPtr):cs.system.runtime.interopservices.GCHandle {})
	/**
	 * A  is stored using an internal integer representation.
	 * @param value An  that indicates the handle for which the conversion is required.
	 * @return The stored  object using an internal integer representation.
	 */
	static function op_Explicit(value:cs.system.runtime.interopservices.GCHandle):cs.system.IntPtr;
	/**
	 * Returns a value indicating whether two  objects are not equal.
	 * @param a A  object to compare with the  parameter.
	 * @param b A  object to compare with the  parameter.
	 * @return if the  and  parameters are not equal; otherwise, .
	 */
	static function op_Inequality(a:cs.system.runtime.interopservices.GCHandle, b:cs.system.runtime.interopservices.GCHandle):Bool;
	/**
	 * Returns the internal integer representation of a  object.
	 * @param value A  object to retrieve an internal integer representation from.
	 * @return An  object that represents a  object.
	 */
	static function ToIntPtr(value:cs.system.runtime.interopservices.GCHandle):cs.system.IntPtr;
	/**
	 * Retrieves the address of an object in a  handle.
	 * @return The address of the pinned object as an .
	 */
	function AddrOfPinnedObject():cs.system.IntPtr;
	/**
	 * Determines whether the specified  object is equal to the current  object.
	 * @param o The  object to compare with the current  object.
	 * @return if the specified  object is equal to the current  object; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/** Releases a . */
	function Free():Void;
	/**
	 * Returns an identifier for the current  object.
	 * @return An identifier for the current  object.
	 */
	function GetHashCode():Int;
}
