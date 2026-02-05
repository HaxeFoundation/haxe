package cs.system.xml.serialization;

/** Instructs the  not to serialize the public field or public read/write property value. */
@:native("System.Xml.Serialization.SoapIgnoreAttribute")
extern class SoapIgnoreAttribute extends cs.system.Attribute {
	function new():Void;
}
