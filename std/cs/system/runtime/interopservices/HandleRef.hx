package cs.system.runtime.interopservices;

/** Wraps a managed object holding a handle to a resource that is passed to unmanaged code using platform invoke. */
@:native("System.Runtime.InteropServices.HandleRef")
extern class HandleRef extends cs.system.ValueType {
	/**
	 * Gets the handle to a resource.
	 * @return The handle to a resource.
	 */
	var Handle(default, never):cs.system.IntPtr;
	/**
	 * Gets the object holding the handle to a resource.
	 * @return The object holding the handle to a resource.
	 */
	var Wrapper(default, never):Dynamic;
	function new(wrapper:Dynamic, handle:cs.system.IntPtr):Void;
	/**
	 * Returns the handle to a resource of the specified  object.
	 * @param value The object that needs a handle.
	 * @return The handle to a resource of the specified  object.
	 */
	static function op_Explicit(value:cs.system.runtime.interopservices.HandleRef):cs.system.IntPtr;
	/**
	 * Returns the internal integer representation of a  object.
	 * @param value A  object to retrieve an internal integer representation from.
	 * @return An  object that represents a  object.
	 */
	static function ToIntPtr(value:cs.system.runtime.interopservices.HandleRef):cs.system.IntPtr;
}
