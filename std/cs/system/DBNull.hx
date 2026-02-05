package cs.system;

/** Represents a nonexistent value. This class cannot be inherited. */
@:native("System.DBNull")
extern class DBNull {
	/** Represents the sole instance of the  class. */
	static var Value(default, never):cs.system.DBNull;
	/**
	 * Implements the  interface and returns the data needed to serialize the  object.
	 * @param info A  object containing information required to serialize the  object.
	 * @param context A  object containing the source and destination of the serialized
	 * stream associated with the  object.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Gets the  value for .
	 * @return The  value for , which is .
	 */
	function GetTypeCode():cs.system.TypeCode;
	@:overload(function():String {})
	/**
	 * Returns an empty string ().
	 * @return An empty string ().
	 */
	function ToString(provider:cs.system.IFormatProvider):String;
}
