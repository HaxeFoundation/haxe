package cs.system;

/** Indicates that the value of a static field is unique for a particular context. */
@:native("System.ContextStaticAttribute")
extern class ContextStaticAttribute extends cs.system.Attribute {
	function new():Void;
}
