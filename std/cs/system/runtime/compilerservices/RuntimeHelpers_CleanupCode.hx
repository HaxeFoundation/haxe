package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.RuntimeHelpers.CleanupCode")
extern class RuntimeHelpers_CleanupCode extends cs.system.MulticastDelegate {
	function new(func:(userData:Dynamic, exceptionThrown:Bool)->Void):Void;
	function Invoke(userData:Dynamic, exceptionThrown:Bool):Void;
}
