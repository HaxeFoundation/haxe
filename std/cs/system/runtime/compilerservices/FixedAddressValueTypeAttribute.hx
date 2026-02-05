package cs.system.runtime.compilerservices;

/** Fixes the address of a static value type field throughout its lifetime. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.FixedAddressValueTypeAttribute")
extern class FixedAddressValueTypeAttribute extends cs.system.Attribute {
	function new():Void;
}
