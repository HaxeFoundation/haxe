package cs.system.runtime.compilerservices;

/** Allows you to obtain the line number in the source file at which the method is called. */
@:native("System.Runtime.CompilerServices.CallerLineNumberAttribute")
extern class CallerLineNumberAttribute extends cs.system.Attribute {
	function new():Void;
}
