package cs.system.diagnostics.contracts;

/** Contains static methods for representing program contracts such as preconditions, postconditions, and object invariants. */
@:native("System.Diagnostics.Contracts.Contract")
extern class Contract {
	@:overload(function(condition:Bool):Void {})
	/**
	 * Checks for a condition; if the condition is , follows the escalation policy set
	 * for the analyzer.
	 * @param condition The conditional expression to test.
	 */
	static function Assert(condition:Bool, userMessage:String):Void;
	@:overload(function(condition:Bool):Void {})
	/**
	 * Instructs code analysis tools to assume that the specified condition is , even
	 * if it cannot be statically proven to always be .
	 * @param condition The conditional expression to assume .
	 */
	static function Assume(condition:Bool, userMessage:String):Void;
	/** Marks the end of the contract section when a method's contracts contain only preconditions in the -- form. */
	static function EndContractBlock():Void;
	@:overload(function(condition:Bool):Void {})
	/**
	 * Specifies a postcondition contract for the enclosing method or property.
	 * @param condition The conditional expression to test. The expression may include
	 * , , and  values.
	 */
	static function Ensures(condition:Bool, userMessage:String):Void;
	@:overload(function<TException>(condition:Bool):Void {})
	/**
	 * Specifies a postcondition contract for the enclosing method or property, based
	 * on the provided exception and condition.
	 * @param TException The type of exception that invokes the postcondition check.
	 * @param condition The conditional expression to test.
	 */
	static function EnsuresOnThrow<TException>(condition:Bool, userMessage:String):Void;
	@:overload(function<T>(collection:cs.system.collections.generic.IEnumerable<T>, predicate:cs.system.Predicate<T>):Bool {})
	/**
	 * Determines whether a specified test is true for any integer within a range of
	 * integers.
	 * @param fromInclusive The first integer to pass to .
	 * @param toExclusive One more than the last integer to pass to .
	 * @param predicate The function to evaluate for any value of the integer in the
	 * specified range.
	 * @return if  returns  for any integer starting from  to  - 1.
	 */
	static function Exists(fromInclusive:Int, toExclusive:Int, predicate:cs.system.Predicate<Int>):Bool;
	@:overload(function<T>(collection:cs.system.collections.generic.IEnumerable<T>, predicate:cs.system.Predicate<T>):Bool {})
	/**
	 * Determines whether a particular condition is valid for all integers in a
	 * specified range.
	 * @param fromInclusive The first integer to pass to .
	 * @param toExclusive One more than the last integer to pass to .
	 * @param predicate The function to evaluate for the existence of the integers in
	 * the specified range.
	 * @return if  returns  for all integers starting from  to  - 1.
	 */
	static function ForAll(fromInclusive:Int, toExclusive:Int, predicate:cs.system.Predicate<Int>):Bool;
	@:overload(function(condition:Bool):Void {})
	/**
	 * Specifies an invariant contract for the enclosing method or property.
	 * @param condition The conditional expression to test.
	 */
	static function Invariant(condition:Bool, userMessage:String):Void;
	/**
	 * Represents values as they were at the start of a method or property.
	 * @param T The type of value.
	 * @param value The value to represent (field or parameter).
	 * @return The value of the parameter or field at the start of a method or
	 * property.
	 */
	static function OldValue<T>(value:T):T;
	@:overload(function(condition:Bool):Void {})
	@:overload(function<TException>(condition:Bool):Void {})
	@:overload(function(condition:Bool, userMessage:String):Void {})
	/**
	 * Specifies a precondition contract for the enclosing method or property.
	 * @param condition The conditional expression to test.
	 */
	static function Requires<TException>(condition:Bool, userMessage:String):Void;
	/**
	 * Represents the return value of a method or property.
	 * @param T Type of return value of the enclosing method or property.
	 * @return Return value of the enclosing method or property.
	 */
	static function Result<T>():T;
	/**
	 * Represents the final (output) value of an  parameter when returning from a
	 * method.
	 * @param T The type of the  parameter.
	 * @param value The  parameter.
	 * @return The output value of the  parameter.
	 */
	static function ValueAtReturn<T>(value:cs.Ref<T>):T;
}
