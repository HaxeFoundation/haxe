package cs.system.runtime.compilerservices;

/** Indicates that any private members contained in an assembly's types are not available to reflection. */
@:native("System.Runtime.CompilerServices.DisablePrivateReflectionAttribute")
extern class DisablePrivateReflectionAttribute extends cs.system.Attribute {
	function new():Void;
}
