package cs.system.xml.serialization;

/**
 * Delegate used by the  class for deserialization of types from SOAP-encoded,
 * non-root XML data.
 * @return The object returned by the callback.
 */
@:native("System.Xml.Serialization.XmlSerializationReadCallback")
extern class XmlSerializationReadCallback extends cs.system.MulticastDelegate {
	function new(func:()->Dynamic):Void;
	function Invoke():Dynamic;
}
