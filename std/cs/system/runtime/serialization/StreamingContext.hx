package cs.system.runtime.serialization;

/** Describes the source and destination of a given serialized stream, and provides an additional caller-defined context. */
@:native("System.Runtime.Serialization.StreamingContext")
extern class StreamingContext extends cs.system.ValueType {
	/**
	 * Gets context specified as part of the additional context.
	 * @return The context specified as part of the additional context.
	 */
	var Context(default, never):Dynamic;
	/**
	 * Gets the source or destination of the transmitted data.
	 * @return During serialization, the destination of the transmitted data. During
	 * deserialization, the source of the data.
	 */
	var State(default, never):cs.system.runtime.serialization.StreamingContextStates;
	@:overload(function(state:cs.system.runtime.serialization.StreamingContextStates):Void {})
	function new(state:cs.system.runtime.serialization.StreamingContextStates, additional:Dynamic):Void;
	/**
	 * Determines whether two  instances contain the same values.
	 * @param obj An object to compare with the current instance.
	 * @return if the specified object is an instance of  and equals the value of the
	 * current instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns a hash code of this object.
	 * @return The  value that contains the source or destination of the serialization
	 * for this .
	 */
	function GetHashCode():Int;
}
