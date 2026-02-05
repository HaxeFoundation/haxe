package cs.system;

/** Marks the program elements that are no longer in use. This class cannot be inherited. */
@:native("System.ObsoleteAttribute")
extern class ObsoleteAttribute extends cs.system.Attribute {
	/**
	 * Gets a Boolean value indicating whether the compiler will treat usage of the
	 * obsolete program element as an error.
	 * @return if the obsolete element usage is considered an error; otherwise, . The
	 * default is .
	 */
	var IsError(default, never):Bool;
	/**
	 * Gets the workaround message, including a description of the alternative program
	 * elements.
	 * @return The workaround text string.
	 */
	var Message(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, error:Bool):Void;
}
