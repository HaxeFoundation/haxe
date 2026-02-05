package cs.system.runtime.compilerservices;

/** Allows you to obtain the method or property name of the caller to the method. */
@:native("System.Runtime.CompilerServices.CallerMemberNameAttribute")
extern class CallerMemberNameAttribute extends cs.system.Attribute {
	function new():Void;
}
