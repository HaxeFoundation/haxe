package cs.system.runtime.compilerservices;

/** Indicates that a switch expression that was non-exhaustive failed to match its input at runtime. The exception optionally contains an object representing the unmatched value. */
@:native("System.Runtime.CompilerServices.SwitchExpressionException")
extern class SwitchExpressionException extends cs.system.InvalidOperationException {
	/**
	 * Gets the unmatched value associated with the exception.
	 * @return The unmatched value causing the exception.
	 */
	var UnmatchedValue(default, never):Dynamic;
	@:overload(function():Void {})
	@:overload(function(innerException:cs.system.Exception):Void {})
	@:overload(function(unmatchedValue:Dynamic):Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
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
