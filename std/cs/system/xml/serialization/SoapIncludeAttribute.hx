package cs.system.xml.serialization;

/** Allows the  to recognize a type when it serializes or deserializes an object as encoded SOAP XML. */
@:native("System.Xml.Serialization.SoapIncludeAttribute")
extern class SoapIncludeAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the type of the object to use when serializing or deserializing an
	 * object.
	 * @return The type of the object to include.
	 */
	var Type(default, default):cs.system.Type;
	function new(type:cs.system.Type):Void;
}
