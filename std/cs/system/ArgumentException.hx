package cs.system;

/** The exception that is thrown when one of the arguments provided to a method is not valid. */
@:native("System.ArgumentException")
extern class ArgumentException extends cs.system.SystemException {
	/**
	 * Gets the name of the parameter that causes this exception.
	 * @return The parameter name.
	 */
	var ParamName(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(message:String, paramName:String):Void {})
	function new(message:String, paramName:String, innerException:cs.system.Exception):Void;
	/**
	 * Sets the  object with the parameter name and additional exception information.
	 * @param info The object that holds the serialized object data.
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
