package cs.system.runtime.serialization;

/** Indicates that a class is to be notified when deserialization of the entire object graph has been completed. Note that this interface is not called when deserializing with the XmlSerializer (System.Xml.Serialization.XmlSerializer). */
@:native("System.Runtime.Serialization.IDeserializationCallback")
extern interface IDeserializationCallback {
	/**
	 * Runs when the entire object graph has been deserialized.
	 * @param sender The object that initiated the callback. The functionality for this
	 * parameter is not currently implemented.
	 */
	function OnDeserialization(sender:Dynamic):Void;
}
