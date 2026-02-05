package cs.system;

/** Represents the base class for classes that contain event data, and provides a value to use for events that do not include event data. */
@:native("System.EventArgs")
extern class EventArgs {
	/** Provides a value to use with events that do not have event data. */
	static var Empty(default, never):cs.system.EventArgs;
	function new():Void;
}
