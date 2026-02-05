package cs.system.diagnostics;

/** Enables communication with a debugger. This class cannot be inherited. */
@:native("System.Diagnostics.Debugger")
extern class Debugger {
	/** Represents the default category of message with a constant. */
	static var DefaultCategory(default, never):String;
	/**
	 * Gets a value that indicates whether a debugger is attached to the process.
	 * @return if a debugger is attached; otherwise, .
	 */
	static var IsAttached(default, never):Bool;
	/** Signals a breakpoint to an attached debugger. */
	static function Break():Void;
	/**
	 * Checks to see if logging is enabled by an attached debugger.
	 * @return if a debugger is attached and logging is enabled; otherwise, . The
	 * attached debugger is the registered managed debugger in the  registry key. For
	 * more information on this key, see Enabling JIT-Attach Debugging.
	 */
	static function IsLogging():Bool;
	/**
	 * Launches and attaches a debugger to the process.
	 * @return if the startup is successful or if the debugger is already attached;
	 * otherwise, .
	 */
	static function Launch():Bool;
	/**
	 * Posts a message for the attached debugger.
	 * @param level A description of the importance of the message.
	 * @param category The category of the message.
	 * @param message The message to show.
	 */
	static function Log(level:Int, category:String, message:String):Void;
	/** Notifies a debugger that execution is about to enter a path that involves a cross-thread dependency. */
	static function NotifyOfCrossThreadDependency():Void;
}
