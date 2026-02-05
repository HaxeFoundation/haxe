package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.RuntimeHelpers.TryCode")
extern class RuntimeHelpers_TryCode extends cs.system.MulticastDelegate {
	function new(func:(userData:Dynamic)->Void):Void;
	function Invoke(userData:Dynamic):Void;
}
