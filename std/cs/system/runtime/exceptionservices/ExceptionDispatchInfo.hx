package cs.system.runtime.exceptionservices;

/** Represents an exception whose state is captured at a certain point in code. */
@:native("System.Runtime.ExceptionServices.ExceptionDispatchInfo")
extern class ExceptionDispatchInfo {
	/**
	 * Gets the exception that is represented by the current instance.
	 * @return The exception that is represented by the current instance.
	 */
	var SourceException(default, never):cs.system.Exception;
	/**
	 * Creates an  object that represents the specified exception at the current point
	 * in code.
	 * @param source The exception whose state is captured, and which is represented by
	 * the returned object.
	 * @return An object that represents the specified exception at the current point
	 * in code.
	 */
	static function Capture(source:cs.system.Exception):cs.system.runtime.exceptionservices.ExceptionDispatchInfo;
	/** Throws the exception that is represented by the current  object, after restoring the state that was saved when the exception was captured. */
	static function Throw(source:cs.system.Exception):Void;
	/** Throws the exception that is represented by the current  object, after restoring the state that was saved when the exception was captured. */
	function Throw():Void;
}
