package cs.system.runtime.interopservices;

/** Specifies the method to call when you register an assembly for use from COM; this enables the execution of user-written code during the registration process. */
@:native("System.Runtime.InteropServices.ComRegisterFunctionAttribute")
extern class ComRegisterFunctionAttribute extends cs.system.Attribute {
	function new():Void;
}
