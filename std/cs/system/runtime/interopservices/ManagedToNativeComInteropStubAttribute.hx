package cs.system.runtime.interopservices;

/** Provides support for user customization of interop stubs in managed-to-COM interop scenarios. */
@:native("System.Runtime.InteropServices.ManagedToNativeComInteropStubAttribute")
extern class ManagedToNativeComInteropStubAttribute extends cs.system.Attribute {
	/**
	 * Gets the class that contains the required stub method.
	 * @return The class that contains the customized interop stub.
	 */
	var ClassType(default, never):cs.system.Type;
	/**
	 * Gets the name of the stub method.
	 * @return The name of a customized interop stub.
	 */
	var MethodName(default, never):String;
	function new(classType:cs.system.Type, methodName:String):Void;
}
