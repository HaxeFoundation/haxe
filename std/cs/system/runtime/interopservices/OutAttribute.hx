package cs.system.runtime.interopservices;

/** Indicates that data should be marshaled from callee back to caller. */
@:native("System.Runtime.InteropServices.OutAttribute")
extern class OutAttribute extends cs.system.Attribute {
	function new():Void;
}
