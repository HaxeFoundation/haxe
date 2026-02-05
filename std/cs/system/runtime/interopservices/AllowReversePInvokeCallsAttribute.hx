package cs.system.runtime.interopservices;

/** Allows an unmanaged method to call a managed method. */
@:native("System.Runtime.InteropServices.AllowReversePInvokeCallsAttribute")
extern class AllowReversePInvokeCallsAttribute extends cs.system.Attribute {
	function new():Void;
}
