package cs.system.reflection;

/** Represents a clause in a structured exception-handling block. */
@:native("System.Reflection.ExceptionHandlingClause")
extern class ExceptionHandlingClause {
	/**
	 * Gets the type of exception handled by this clause.
	 * @return A  object that represents that type of exception handled by this clause,
	 * or  if the  property is  or .
	 */
	var CatchType(default, never):cs.system.Type;
	/**
	 * Gets the offset within the method body, in bytes, of the user-supplied filter
	 * code.
	 * @return The offset within the method body, in bytes, of the user-supplied filter
	 * code. The value of this property has no meaning if the  property has any value
	 * other than .
	 */
	var FilterOffset(default, never):Int;
	/**
	 * Gets a value indicating whether this exception-handling clause is a finally
	 * clause, a type-filtered clause, or a user-filtered clause.
	 * @return An  value that indicates what kind of action this clause performs.
	 */
	var Flags(default, never):cs.system.reflection.ExceptionHandlingClauseOptions;
	/**
	 * Gets the length, in bytes, of the body of this exception-handling clause.
	 * @return An integer that represents the length, in bytes, of the MSIL that forms
	 * the body of this exception-handling clause.
	 */
	var HandlerLength(default, never):Int;
	/**
	 * Gets the offset within the method body, in bytes, of this exception-handling
	 * clause.
	 * @return An integer that represents the offset within the method body, in bytes,
	 * of this exception-handling clause.
	 */
	var HandlerOffset(default, never):Int;
	/**
	 * The total length, in bytes, of the try block that includes this
	 * exception-handling clause.
	 * @return The total length, in bytes, of the try block that includes this
	 * exception-handling clause.
	 */
	var TryLength(default, never):Int;
	/**
	 * The offset within the method, in bytes, of the try block that includes this
	 * exception-handling clause.
	 * @return An integer that represents the offset within the method, in bytes, of
	 * the try block that includes this exception-handling clause.
	 */
	var TryOffset(default, never):Int;
	/**
	 * A string representation of the exception-handling clause.
	 * @return A string that lists appropriate property values for the filter clause
	 * type.
	 */
	function ToString():String;
}
