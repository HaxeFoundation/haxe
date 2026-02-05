package cs.system.runtime.interopservices;

/** Indicates that information was lost about a class or interface when it was imported from a type library to an assembly. */
@:native("System.Runtime.InteropServices.ComConversionLossAttribute")
extern class ComConversionLossAttribute extends cs.system.Attribute {
	function new():Void;
}
