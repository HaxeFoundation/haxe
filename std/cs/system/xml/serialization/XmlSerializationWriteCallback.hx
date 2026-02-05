package cs.system.xml.serialization;

/**
 * Delegate that is used by the  class for serialization of types from
 * SOAP-encoded, non-root XML data.
 * @param o The object being serialized.
 */
@:native("System.Xml.Serialization.XmlSerializationWriteCallback")
extern class XmlSerializationWriteCallback extends cs.system.MulticastDelegate {
	function new(func:(o:Dynamic)->Void):Void;
	function Invoke(o:Dynamic):Void;
}
