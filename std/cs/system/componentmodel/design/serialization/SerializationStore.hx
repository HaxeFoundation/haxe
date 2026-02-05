package cs.system.componentmodel.design.serialization;

/** Provides the base class for storing serialization data for the . */
@:native("System.ComponentModel.Design.Serialization.SerializationStore")
extern class SerializationStore {
	/**
	 * Gets a collection of errors that occurred during serialization or
	 * deserialization.
	 * @return An  that contains errors that occurred during serialization or
	 * deserialization.
	 */
	var Errors(default, never):cs.system.collections.ICollection;
	/** Closes the serialization store. */
	function Close():Void;
	/**
	 * Saves the store to the given stream.
	 * @param stream The stream to which the store will be serialized.
	 */
	function Save(stream:cs.system.io.Stream):Void;
}
