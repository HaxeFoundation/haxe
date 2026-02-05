package cs.system.runtime.serialization;

/** Provides data for the  event. */
@:native("System.Runtime.Serialization.SafeSerializationEventArgs")
extern class SafeSerializationEventArgs extends cs.system.EventArgs {
	/**
	 * Gets or sets an object that describes the source and destination of a serialized
	 * stream.
	 * @return An object that describes the source and destination of a serialized
	 * stream.
	 */
	var StreamingContext(default, never):cs.system.runtime.serialization.StreamingContext;
	/**
	 * Stores the state of the exception.
	 * @param serializedState A state object that is serialized with the instance.
	 */
	function AddSerializedState(serializedState:cs.system.runtime.serialization.ISafeSerializationData):Void;
}
