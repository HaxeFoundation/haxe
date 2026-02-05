package cs.system.diagnostics.symbolstore;

/** The  structure is an object representation of a token that represents symbolic information. */
@:native("System.Diagnostics.SymbolStore.SymbolToken")
extern class SymbolToken extends cs.system.ValueType {
	function new(val:Int):Void;
	/**
	 * Returns a value indicating whether two  objects are equal.
	 * @param a A  structure.
	 * @param b A  structure.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(a:cs.system.diagnostics.symbolstore.SymbolToken, b:cs.system.diagnostics.symbolstore.SymbolToken):Bool;
	/**
	 * Returns a value indicating whether two  objects are not equal.
	 * @param a A  structure.
	 * @param b A  structure.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(a:cs.system.diagnostics.symbolstore.SymbolToken, b:cs.system.diagnostics.symbolstore.SymbolToken):Bool;
	@:overload(function(obj:cs.system.diagnostics.symbolstore.SymbolToken):Bool {})
	/**
	 * Determines whether  is equal to this instance.
	 * @param obj The  to check.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Generates the hash code for the current token.
	 * @return The hash code for the current token.
	 */
	function GetHashCode():Int;
	/**
	 * Gets the value of the current token.
	 * @return The value of the current token.
	 */
	function GetToken():Int;
}
