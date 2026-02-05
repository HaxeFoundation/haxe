package cs.system.runtime.interopservices;

/** Indicates that data should be marshaled from the caller to the callee, but not back to the caller. */
@:native("System.Runtime.InteropServices.InAttribute")
extern class InAttribute extends cs.system.Attribute {
	function new():Void;
}
