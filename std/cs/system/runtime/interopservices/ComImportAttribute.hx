package cs.system.runtime.interopservices;

/** Indicates that the attributed type was previously defined in COM. */
@:native("System.Runtime.InteropServices.ComImportAttribute")
extern class ComImportAttribute extends cs.system.Attribute {
	function new():Void;
}
