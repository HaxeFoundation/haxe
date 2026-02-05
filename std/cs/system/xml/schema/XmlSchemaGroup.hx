package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class defines groups at the  level that are referenced from the complex types. It groups a set of element declarations so that they can be incorporated as a group into complex type definitions. */
@:native("System.Xml.Schema.XmlSchemaGroup")
extern class XmlSchemaGroup extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets the name of the schema group.
	 * @return The name of the schema group.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets one of the , , or  classes.
	 * @return One of the , , or  classes.
	 */
	var Particle(default, default):cs.system.xml.schema.XmlSchemaGroupBase;
	/**
	 * Gets the qualified name of the schema group.
	 * @return An  object representing the qualified name of the schema group.
	 */
	var QualifiedName(default, never):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
