package cs.system;

/** Indicates that the value of a static field is unique for each thread. */
@:native("System.ThreadStaticAttribute")
extern class ThreadStaticAttribute extends cs.system.Attribute {
	function new():Void;
}
