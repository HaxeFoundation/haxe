package cs.system.runtime.serialization;

/** Provides a data structure to store extra data encountered by the  during deserialization of a type marked with the  attribute. */
@:native("System.Runtime.Serialization.IExtensibleDataObject")
extern interface IExtensibleDataObject {
	/**
	 * Gets or sets the structure that contains extra data.
	 * @return An  that contains data that is not recognized as belonging to the data
	 * contract.
	 */
	var ExtensionData(default, default):cs.system.runtime.serialization.ExtensionDataObject;
}
