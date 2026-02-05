package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class is for complex types with complex content model derived by extension. It extends the complex type by adding attributes or elements. */
@:native("System.Xml.Schema.XmlSchemaComplexContentExtension")
extern class XmlSchemaComplexContentExtension extends cs.system.xml.schema.XmlSchemaContent {
	/**
	 * Gets or sets the  component of the complex content model.
	 * @return The  component of the complex content model.
	 */
	var AnyAttribute(default, default):cs.system.xml.schema.XmlSchemaAnyAttribute;
	/**
	 * Gets the collection of attributes for the complex content. Contains  and 
	 * elements.
	 * @return The collection of attributes for the complex content.
	 */
	var Attributes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the name of the complex type from which this type is derived by
	 * extension.
	 * @return The name of the complex type from which this type is derived by
	 * extension.
	 */
	var BaseTypeName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets one of the , , , or  classes.
	 * @return One of the , , , or  classes.
	 */
	var Particle(default, default):cs.system.xml.schema.XmlSchemaParticle;
	function new():Void;
}
