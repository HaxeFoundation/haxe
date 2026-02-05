package cs.system.xml.schema;

/** Represents the root class for the Xml schema object model hierarchy and serves as a base class for classes such as the  class. */
@:native("System.Xml.Schema.XmlSchemaObject")
extern class XmlSchemaObject {
	/**
	 * Gets or sets the line number in the file to which the  element refers.
	 * @return The line number.
	 */
	var LineNumber(default, default):Int;
	/**
	 * Gets or sets the line position in the file to which the  element refers.
	 * @return The line position.
	 */
	var LinePosition(default, default):Int;
	/**
	 * Gets or sets the  to use with this schema object.
	 * @return The  property for the schema object.
	 */
	var Namespaces(default, default):cs.system.xml.serialization.XmlSerializerNamespaces;
	/**
	 * Gets or sets the parent of this .
	 * @return The parent  of this .
	 */
	var Parent(default, default):cs.system.xml.schema.XmlSchemaObject;
	/**
	 * Gets or sets the source location for the file that loaded the schema.
	 * @return The source location (URI) for the file.
	 */
	var SourceUri(default, default):String;
}
