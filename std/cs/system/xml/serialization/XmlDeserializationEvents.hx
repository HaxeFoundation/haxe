package cs.system.xml.serialization;

/** Contains fields that can be used to pass event delegates to a thread-safe  method of the . */
@:native("System.Xml.Serialization.XmlDeserializationEvents")
extern class XmlDeserializationEvents extends cs.system.ValueType {
	var OnUnknownAttribute(default, default):cs.system.xml.serialization.XmlAttributeEventHandler;
	var OnUnknownElement(default, default):cs.system.xml.serialization.XmlElementEventHandler;
	var OnUnknownNode(default, default):cs.system.xml.serialization.XmlNodeEventHandler;
	var OnUnreferencedObject(default, default):cs.system.xml.serialization.UnreferencedObjectEventHandler;
}
