package cs.system.componentmodel;

/** Specifies an exception that is handled as a warning instead of an error. */
@:native("System.ComponentModel.WarningException")
extern class WarningException extends cs.system.SystemException {
	/**
	 * Gets the Help topic associated with the warning.
	 * @return The Help topic associated with the warning.
	 */
	var HelpTopic(default, never):String;
	/**
	 * Gets the Help file associated with the warning.
	 * @return The Help file associated with the warning.
	 */
	var HelpUrl(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(message:String, helpUrl:String):Void {})
	function new(message:String, helpUrl:String, helpTopic:String):Void;
	/**
	 * Sets the  with the parameter name and additional exception information.
	 * @param info Stores the data that was being used to serialize or deserialize the
	 * object that the  was serializing or deserializing.
	 * @param context Describes the source and destination of the stream that generated
	 * the exception, as well as a means for serialization to retain that context and
	 * an additional caller-defined context.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
