package cs.system.runtime.interopservices;

/** Specifies the method to call when you unregister an assembly for use from COM; this allows for the execution of user-written code during the unregistration process. */
@:native("System.Runtime.InteropServices.ComUnregisterFunctionAttribute")
extern class ComUnregisterFunctionAttribute extends cs.system.Attribute {
	function new():Void;
}
