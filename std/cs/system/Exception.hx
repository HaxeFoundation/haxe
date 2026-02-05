package cs.system;

/** Represents errors that occur during application execution. */
@:native("System.Exception")
extern class Exception {
	/**
	 * Gets a collection of key/value pairs that provide additional user-defined
	 * information about the exception.
	 * @return An object that implements the  interface and contains a collection of
	 * user-defined key/value pairs. The default is an empty collection.
	 */
	var Data(default, never):cs.system.collections.IDictionary;
	/**
	 * Gets or sets a link to the help file associated with this exception.
	 * @return The Uniform Resource Name (URN) or Uniform Resource Locator (URL).
	 */
	var HelpLink(default, default):String;
	/**
	 * Gets or sets HRESULT, a coded numerical value that is assigned to a specific
	 * exception.
	 * @return The HRESULT value.
	 */
	var HResult(default, default):Int;
	/**
	 * Gets the  instance that caused the current exception.
	 * @return An object that describes the error that caused the current exception.
	 * The  property returns the same value as was passed into the  constructor, or  if
	 * the inner exception value was not supplied to the constructor. This property is
	 * read-only.
	 */
	var InnerException(default, never):cs.system.Exception;
	/**
	 * Gets a message that describes the current exception.
	 * @return The error message that explains the reason for the exception, or an
	 * empty string ("").
	 */
	var Message(default, never):String;
	/**
	 * Gets or sets the name of the application or the object that causes the error.
	 * @return The name of the application or the object that causes the error.
	 */
	var Source(default, default):String;
	/**
	 * Gets a string representation of the immediate frames on the call stack.
	 * @return A string that describes the immediate frames of the call stack.
	 */
	var StackTrace(default, never):String;
	/**
	 * Gets the method that throws the current exception.
	 * @return The  that threw the current exception.
	 */
	var TargetSite(default, never):cs.system.reflection.MethodBase;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
	/**
	 * When overridden in a derived class, returns the  that is the root cause of one
	 * or more subsequent exceptions.
	 * @return The first exception thrown in a chain of exceptions. If the  property of
	 * the current exception is a null reference ( in Visual Basic), this property
	 * returns the current exception.
	 */
	function GetBaseException():cs.system.Exception;
	/**
	 * When overridden in a derived class, sets the  with information about the
	 * exception.
	 * @param info The  that holds the serialized object data about the exception being
	 * thrown.
	 * @param context The  that contains contextual information about the source or
	 * destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Gets the runtime type of the current instance.
	 * @return A  object that represents the exact runtime type of the current
	 * instance.
	 */
	function GetType():cs.system.Type;
	/**
	 * Creates and returns a string representation of the current exception.
	 * @return A string representation of the current exception.
	 */
	function ToString():String;
}
