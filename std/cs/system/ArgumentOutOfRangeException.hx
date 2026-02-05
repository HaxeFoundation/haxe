package cs.system;

/** The exception that is thrown when the value of an argument is outside the allowable range of values as defined by the invoked method. */
@:native("System.ArgumentOutOfRangeException")
extern class ArgumentOutOfRangeException extends cs.system.ArgumentException {
	/**
	 * Gets the argument value that causes this exception.
	 * @return An  that contains the value of the parameter that caused the current .
	 */
	var ActualValue(default, never):Dynamic;
	@:overload(function():Void {})
	@:overload(function(paramName:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(paramName:String, message:String):Void {})
	function new(paramName:String, actualValue:Dynamic, message:String):Void;
	/**
	 * Sets the  object with the invalid argument value and additional exception
	 * information.
	 * @param info The object that holds the serialized object data.
	 * @param context An object that describes the source or destination of the
	 * serialized data.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
