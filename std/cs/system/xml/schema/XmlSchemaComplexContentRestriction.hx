package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class is for complex types with a complex content model derived by restriction. It restricts the contents of the complex type to a subset of the inherited complex type. */
@:native("System.Xml.Schema.XmlSchemaComplexContentRestriction")
extern class XmlSchemaComplexContentRestriction extends cs.system.xml.schema.XmlSchemaContent {
	/**
	 * Gets or sets the  component of the complex content model.
	 * @return The  component of the complex content model.
	 */
	var AnyAttribute(default, default):cs.system.xml.schema.XmlSchemaAnyAttribute;
	/**
	 * Gets the collection of attributes for the complex type. Contains the  and 
	 * elements.
	 * @return The collection of attributes for the complex type.
	 */
	var Attributes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the name of a complex type from which this type is derived by
	 * restriction.
	 * @return The name of the complex type from which this type is derived by
	 * restriction.
	 */
	var BaseTypeName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets one of the , , , or  classes.
	 * @return One of the , , , or  classes.
	 */
	var Particle(default, default):cs.system.xml.schema.XmlSchemaParticle;
	function new():Void;
}
