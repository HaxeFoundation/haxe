package cs.system.runtime.serialization;

/** Enables serialization of custom exception data in security-transparent code. */
@:native("System.Runtime.Serialization.ISafeSerializationData")
extern interface ISafeSerializationData {
	/**
	 * This method is called when the instance is deserialized.
	 * @param deserialized An object that contains the state of the instance.
	 */
	function CompleteDeserialization(deserialized:Dynamic):Void;
}
