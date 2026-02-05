package cs.system.runtime.compilerservices;

/** Allows you to obtain the full path of the source file that contains the caller. This is the file path at the time of compile. */
@:native("System.Runtime.CompilerServices.CallerFilePathAttribute")
extern class CallerFilePathAttribute extends cs.system.Attribute {
	function new():Void;
}
