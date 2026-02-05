package cs.system;

/** Represents a runtime handle for a module. */
@:native("System.ModuleHandle")
extern class ModuleHandle extends cs.system.ValueType {
	/** Represents an empty module handle. */
	static var EmptyHandle(default, never):cs.system.ModuleHandle;
	/**
	 * Gets the metadata stream version.
	 * @return A 32-bit integer representing the metadata stream version. The
	 * high-order two bytes represent the major version number, and the low-order two
	 * bytes represent the minor version number.
	 */
	var MDStreamVersion(default, never):Int;
	/**
	 * Tests whether two  structures are equal.
	 * @param left The  structure to the left of the equality operator.
	 * @param right The  structure to the right of the equality operator.
	 * @return if the  structures are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.ModuleHandle, right:cs.system.ModuleHandle):Bool;
	/**
	 * Tests whether two  structures are unequal.
	 * @param left The  structure to the left of the inequality operator.
	 * @param right The  structure to the right of the inequality operator.
	 * @return if the  structures are unequal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.ModuleHandle, right:cs.system.ModuleHandle):Bool;
	@:overload(function(handle:cs.system.ModuleHandle):Bool {})
	/**
	 * Returns a  value indicating whether the specified  structure is equal to the
	 * current .
	 * @param handle The  structure to be compared with the current .
	 * @return if  is equal to the current  structure; otherwise .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer that is the hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a runtime handle for the field identified by the specified metadata
	 * token.
	 * @param fieldToken A metadata token that identifies a field in the module.
	 * @return A  for the field identified by .
	 */
	function GetRuntimeFieldHandleFromMetadataToken(fieldToken:Int):cs.system.RuntimeFieldHandle;
	/**
	 * Returns a runtime method handle for the method or constructor identified by the
	 * specified metadata token.
	 * @param methodToken A metadata token that identifies a method or constructor in
	 * the module.
	 * @return A  for the method or constructor identified by .
	 */
	function GetRuntimeMethodHandleFromMetadataToken(methodToken:Int):cs.system.RuntimeMethodHandle;
	/**
	 * Returns a runtime type handle for the type identified by the specified metadata
	 * token.
	 * @param typeToken A metadata token that identifies a type in the module.
	 * @return A  for the type identified by .
	 */
	function GetRuntimeTypeHandleFromMetadataToken(typeToken:Int):cs.system.RuntimeTypeHandle;
	@:overload(function(fieldToken:Int):cs.system.RuntimeFieldHandle {})
	/**
	 * Returns a runtime handle for the field identified by the specified metadata
	 * token.
	 * @param fieldToken A metadata token that identifies a field in the module.
	 * @return A  for the field identified by .
	 */
	function ResolveFieldHandle(fieldToken:Int, typeInstantiationContext:cs.NativeArray<cs.system.RuntimeTypeHandle>, methodInstantiationContext:cs.NativeArray<cs.system.RuntimeTypeHandle>):cs.system.RuntimeFieldHandle;
	@:overload(function(methodToken:Int):cs.system.RuntimeMethodHandle {})
	/**
	 * Returns a runtime method handle for the method or constructor identified by the
	 * specified metadata token.
	 * @param methodToken A metadata token that identifies a method or constructor in
	 * the module.
	 * @return A  for the method or constructor identified by .
	 */
	function ResolveMethodHandle(methodToken:Int, typeInstantiationContext:cs.NativeArray<cs.system.RuntimeTypeHandle>, methodInstantiationContext:cs.NativeArray<cs.system.RuntimeTypeHandle>):cs.system.RuntimeMethodHandle;
	@:overload(function(typeToken:Int):cs.system.RuntimeTypeHandle {})
	/**
	 * Returns a runtime type handle for the type identified by the specified metadata
	 * token.
	 * @param typeToken A metadata token that identifies a type in the module.
	 * @return A  for the type identified by .
	 */
	function ResolveTypeHandle(typeToken:Int, typeInstantiationContext:cs.NativeArray<cs.system.RuntimeTypeHandle>, methodInstantiationContext:cs.NativeArray<cs.system.RuntimeTypeHandle>):cs.system.RuntimeTypeHandle;
}
