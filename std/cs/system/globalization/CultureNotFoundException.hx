package cs.system.globalization;

/** The exception that is thrown when a method attempts to construct a culture that is not available. */
@:native("System.Globalization.CultureNotFoundException")
extern class CultureNotFoundException extends cs.system.ArgumentException {
	/**
	 * Gets the culture identifier that cannot be found.
	 * @return The invalid culture identifier.
	 */
	var InvalidCultureId(default, never):Null<Int>;
	/**
	 * Gets the culture name that cannot be found.
	 * @return The invalid culture name.
	 */
	var InvalidCultureName(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	@:overload(function(paramName:String, message:String):Void {})
	@:overload(function(message:String, invalidCultureId:Int, innerException:cs.system.Exception):Void {})
	@:overload(function(paramName:String, invalidCultureId:Int, message:String):Void {})
	@:overload(function(message:String, invalidCultureName:String, innerException:cs.system.Exception):Void {})
	function new(paramName:String, invalidCultureName:String, message:String):Void;
	/**
	 * Sets the  object with the parameter name and additional exception information.
	 * @param info The object that holds the serialized object data.
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
