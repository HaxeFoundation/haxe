package cs.system.xml.serialization;

/** When applied to a type, stores the name of a static method of the type that returns an XML schema and a  (or  for anonymous types) that controls the serialization of the type. */
@:native("System.Xml.Serialization.XmlSchemaProviderAttribute")
extern class XmlSchemaProviderAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a value that determines whether the target class is a wildcard, or
	 * that the schema for the class has contains only an  element.
	 * @return , if the class is a wildcard, or if the schema contains only the 
	 * element; otherwise, .
	 */
	var IsAny(default, default):Bool;
	/**
	 * Gets the name of the static method that supplies the type's XML schema and the
	 * name of its XML Schema data type.
	 * @return The name of the method that is invoked by the XML infrastructure to
	 * return an XML schema.
	 */
	var MethodName(default, never):String;
	function new(methodName:String):Void;
}
