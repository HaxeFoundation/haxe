package cs.system.runtime.compilerservices;

/** Specifies the details of how a method is implemented. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.MethodImplAttribute")
extern class MethodImplAttribute extends cs.system.Attribute {
	/** A  value indicating what kind of implementation is provided for this method. */
	var MethodCodeType:cs.system.runtime.compilerservices.MethodCodeType;
	/**
	 * Gets the  value describing the attributed method.
	 * @return The  value describing the attributed method.
	 */
	var Value(default, never):cs.system.runtime.compilerservices.MethodImplOptions;
	@:overload(function():Void {})
	@:overload(function(value:cs.Int16):Void {})
	function new(methodImplOptions:cs.system.runtime.compilerservices.MethodImplOptions):Void;
}
