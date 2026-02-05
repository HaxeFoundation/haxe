package cs.system;

/** Indicates that an enumeration can be treated as a bit field; that is, a set of flags. */
@:native("System.FlagsAttribute")
extern class FlagsAttribute extends cs.system.Attribute {
	function new():Void;
}
