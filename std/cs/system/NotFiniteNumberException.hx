package cs.system;

/** The exception that is thrown when a floating-point value is positive infinity, negative infinity, or Not-a-Number (NaN). */
@:native("System.NotFiniteNumberException")
extern class NotFiniteNumberException extends cs.system.ArithmeticException {
	/**
	 * Gets the invalid number that is a positive infinity, a negative infinity, or
	 * Not-a-Number (NaN).
	 * @return The invalid number.
	 */
	var OffendingNumber(default, never):Float;
	@:overload(function():Void {})
	@:overload(function(offendingNumber:Float):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, offendingNumber:Float):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, offendingNumber:Float, innerException:cs.system.Exception):Void;
	/**
	 * Sets the  object with the invalid number and additional exception information.
	 * @param info The object that holds the serialized object data.
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
