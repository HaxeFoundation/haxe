package cs.system.runtime.compilerservices;

/** Indicates that a class should be treated as if it has global scope. */
@:native("System.Runtime.CompilerServices.CompilerGlobalScopeAttribute")
extern class CompilerGlobalScopeAttribute extends cs.system.Attribute {
	function new():Void;
}
