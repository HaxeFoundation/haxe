package cs.system.runtime.compilerservices;

/** Stores the value of a  constant in metadata. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.DecimalConstantAttribute")
extern class DecimalConstantAttribute extends cs.system.Attribute {
	/**
	 * Gets the decimal constant stored in this attribute.
	 * @return The decimal constant stored in this attribute.
	 */
	var Value(default, never):cs.system.Decimal;
	@:overload(function(scale:cs.UInt8, sign:cs.UInt8, hi:Int, mid:Int, low:Int):Void {})
	function new(scale:cs.UInt8, sign:cs.UInt8, hi:cs.UInt, mid:cs.UInt, low:cs.UInt):Void;
}
