package cs.system.reflection;

/** Provides access to the metadata and MSIL for the body of a method. */
@:native("System.Reflection.MethodBody")
extern class MethodBody {
	/**
	 * Gets a list that includes all the exception-handling clauses in the method body.
	 * @return An  of  objects representing the exception-handling clauses in the body
	 * of the method.
	 */
	var ExceptionHandlingClauses(default, never):cs.system.collections.generic.IList<cs.system.reflection.ExceptionHandlingClause>;
	/**
	 * Gets a value indicating whether local variables in the method body are
	 * initialized to the default values for their types.
	 * @return if the method body contains code to initialize local variables to  for
	 * reference types, or to the zero-initialized value for value types; otherwise, .
	 */
	var InitLocals(default, never):Bool;
	/**
	 * Gets a metadata token for the signature that describes the local variables for
	 * the method in metadata.
	 * @return An integer that represents the metadata token.
	 */
	var LocalSignatureMetadataToken(default, never):Int;
	/**
	 * Gets the list of local variables declared in the method body.
	 * @return An  of  objects that describe the local variables declared in the method
	 * body.
	 */
	var LocalVariables(default, never):cs.system.collections.generic.IList<cs.system.reflection.LocalVariableInfo>;
	/**
	 * Gets the maximum number of items on the operand stack when the method is
	 * executing.
	 * @return The maximum number of items on the operand stack when the method is
	 * executing.
	 */
	var MaxStackSize(default, never):Int;
	/**
	 * Returns the MSIL for the method body, as an array of bytes.
	 * @return An array of type  that contains the MSIL for the method body.
	 */
	function GetILAsByteArray():cs.NativeArray<cs.UInt8>;
}
