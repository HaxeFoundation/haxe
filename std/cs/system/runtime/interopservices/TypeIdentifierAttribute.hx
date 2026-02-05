package cs.system.runtime.interopservices;

/** Provides support for type equivalence. */
@:native("System.Runtime.InteropServices.TypeIdentifierAttribute")
extern class TypeIdentifierAttribute extends cs.system.Attribute {
	/**
	 * Gets the value of the  parameter that was passed to the  constructor.
	 * @return The value of the constructor's  parameter.
	 */
	var Identifier(default, never):String;
	/**
	 * Gets the value of the  parameter that was passed to the  constructor.
	 * @return The value of the constructor's  parameter.
	 */
	var Scope(default, never):String;
	@:overload(function():Void {})
	function new(scope:String, identifier:String):Void;
}
