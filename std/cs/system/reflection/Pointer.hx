package cs.system.reflection;

/** Provides a wrapper class for pointers. */
@:native("System.Reflection.Pointer")
extern class Pointer {
	/**
	 * Boxes the supplied unmanaged memory pointer and the type associated with that
	 * pointer into a managed  wrapper object. The value and the type are saved so they
	 * can be accessed from the native code during an invocation.
	 * @param ptr The supplied unmanaged memory pointer.
	 * @param type The type associated with the  parameter.
	 * @return A pointer object.
	 */
	static function Box(ptr:cs.Pointer<Void>, type:cs.system.Type):Dynamic;
	/**
	 * Returns the stored pointer.
	 * @param ptr The stored pointer.
	 * @return This method returns void.
	 */
	static function Unbox(ptr:Dynamic):cs.Pointer<Void>;
}
