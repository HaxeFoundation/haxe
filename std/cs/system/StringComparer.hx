package cs.system;

/** Represents a string comparison operation that uses specific case and culture-based or ordinal comparison rules. */
@:native("System.StringComparer")
extern class StringComparer {
	/**
	 * Gets a  object that performs a case-sensitive string comparison using the word
	 * comparison rules of the current culture.
	 * @return A new  object.
	 */
	static var CurrentCulture(default, never):cs.system.StringComparer;
	/**
	 * Gets a  object that performs case-insensitive string comparisons using the word
	 * comparison rules of the current culture.
	 * @return A new object for string comparison.
	 */
	static var CurrentCultureIgnoreCase(default, never):cs.system.StringComparer;
	/**
	 * Gets a  object that performs a case-sensitive string comparison using the word
	 * comparison rules of the invariant culture.
	 * @return A new  object.
	 */
	static var InvariantCulture(default, never):cs.system.StringComparer;
	/**
	 * Gets a  object that performs a case-insensitive string comparison using the word
	 * comparison rules of the invariant culture.
	 * @return A new  object.
	 */
	static var InvariantCultureIgnoreCase(default, never):cs.system.StringComparer;
	/**
	 * Gets a  object that performs a case-sensitive ordinal string comparison.
	 * @return A  object.
	 */
	static var Ordinal(default, never):cs.system.StringComparer;
	/**
	 * Gets a  object that performs a case-insensitive ordinal string comparison.
	 * @return A  object.
	 */
	static var OrdinalIgnoreCase(default, never):cs.system.StringComparer;
	@:overload(function(culture:cs.system.globalization.CultureInfo, ignoreCase:Bool):cs.system.StringComparer {})
	/**
	 * Creates a  object that compares strings according to the rules of a specified
	 * culture.
	 * @param culture A culture whose linguistic rules are used to perform a string
	 * comparison.
	 * @param ignoreCase to specify that comparison operations be case-insensitive;  to
	 * specify that comparison operations be case-sensitive.
	 * @return A new  object that performs string comparisons according to the
	 * comparison rules used by the  parameter and the case rule specified by the 
	 * parameter.
	 */
	static function Create(culture:cs.system.globalization.CultureInfo, options:cs.system.globalization.CompareOptions):cs.system.StringComparer;
	/** @param comparisonType  */
	static function FromComparison(comparisonType:cs.system.StringComparison):cs.system.StringComparer;
	@:overload(function(x:Dynamic, y:Dynamic):Int {})
	/**
	 * When overridden in a derived class, compares two objects and returns an
	 * indication of their relative sort order.
	 * @param x An object to compare to .
	 * @param y An object to compare to .
	 * @return A signed integer that indicates the relative values of  and , as shown
	 * in the following table. Value Meaning Less than zero precedes   in the sort
	 * order. -or- is  and  is not . Zero is equal to . -or- and  are both . Greater
	 * than zero follows  in the sort order. -or- is  and  is not .
	 */
	function Compare(x:String, y:String):Int;
	@:overload(function(x:Dynamic, y:Dynamic):Bool {})
	/**
	 * When overridden in a derived class, indicates whether two objects are equal.
	 * @param x An object to compare to .
	 * @param y An object to compare to .
	 * @return if  and  refer to the same object, or  and  are both the same type of
	 * object and those objects are equal, or both  and  are ; otherwise, .
	 */
	function Equals(x:String, y:String):Bool;
	@:overload(function(obj:Dynamic):Int {})
	/**
	 * When overridden in a derived class, gets the hash code for the specified object.
	 * @param obj An object.
	 * @return A 32-bit signed hash code calculated from the value of the  parameter.
	 */
	function GetHashCode(obj:String):Int;
}
