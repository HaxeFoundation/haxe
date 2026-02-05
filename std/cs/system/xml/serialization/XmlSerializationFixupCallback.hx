package cs.system.xml.serialization;

/**
 * Delegate used by the  class for deserialization of SOAP-encoded XML data.
 * @param fixup An instance of the  class that contains the object to be fixed and
 * the array of string identifiers for the items to fill in.
 */
@:native("System.Xml.Serialization.XmlSerializationFixupCallback")
extern class XmlSerializationFixupCallback extends cs.system.MulticastDelegate {
	function new(func:(fixup:Dynamic)->Void):Void;
	function Invoke(fixup:Dynamic):Void;
}
