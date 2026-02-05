package cs.system.xml.serialization;

/** Contains a mapping of one type to another. */
@:native("System.Xml.Serialization.XmlTypeMapping")
extern class XmlTypeMapping extends cs.system.xml.serialization.XmlMapping {
	/**
	 * The fully qualified type name that includes the namespace (or namespaces) and
	 * type.
	 * @return The fully qualified type name.
	 */
	var TypeFullName(default, never):String;
	/**
	 * Gets the type name of the mapped object.
	 * @return The type name of the mapped object.
	 */
	var TypeName(default, never):String;
	/**
	 * Gets the XML element name of the mapped object.
	 * @return The XML element name of the mapped object. The default is the class name
	 * of the object.
	 */
	var XsdTypeName(default, never):String;
	/**
	 * Gets the XML namespace of the mapped object.
	 * @return The XML namespace of the mapped object. The default is an empty string
	 * ("").
	 */
	var XsdTypeNamespace(default, never):String;
}
