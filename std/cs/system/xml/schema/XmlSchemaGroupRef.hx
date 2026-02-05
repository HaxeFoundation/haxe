package cs.system.xml.schema;

/** Represents the  element with  attribute from the XML Schema as specified by the World Wide Web Consortium (W3C). This class is used within complex types that reference a  defined at the  level. */
@:native("System.Xml.Schema.XmlSchemaGroupRef")
extern class XmlSchemaGroupRef extends cs.system.xml.schema.XmlSchemaParticle {
	/**
	 * Gets one of the , , or  classes, which holds the post-compilation value of the 
	 * property.
	 * @return The post-compilation value of the  property, which is one of the , , or 
	 * classes.
	 */
	var Particle(default, never):cs.system.xml.schema.XmlSchemaGroupBase;
	/**
	 * Gets or sets the name of a group defined in this schema (or another schema
	 * indicated by the specified namespace).
	 * @return The name of a group defined in this schema.
	 */
	var RefName(default, default):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
