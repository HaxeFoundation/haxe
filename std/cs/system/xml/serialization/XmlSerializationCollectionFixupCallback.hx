package cs.system.xml.serialization;

/**
 * Delegate used by the  class for deserialization of SOAP-encoded XML data types
 * that map to collections or enumerations.
 * @param collection The collection into which the collection items array is
 * copied.
 * @param collectionItems An array of items to be copied into the object
 * collection.
 */
@:native("System.Xml.Serialization.XmlSerializationCollectionFixupCallback")
extern class XmlSerializationCollectionFixupCallback extends cs.system.MulticastDelegate {
	function new(func:(collection:Dynamic, collectionItems:Dynamic)->Void):Void;
	function Invoke(collection:Dynamic, collectionItems:Dynamic):Void;
}
