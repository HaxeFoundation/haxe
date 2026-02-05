package cs.system.xml.schema;

/** This class represents the  element from XMLSchema as specified by the World Wide Web Consortium (W3C). */
@:native("System.Xml.Schema.XmlSchemaKeyref")
extern class XmlSchemaKeyref extends cs.system.xml.schema.XmlSchemaIdentityConstraint {
	/**
	 * Gets or sets the name of the key that this constraint refers to in another
	 * simple or complex type.
	 * @return The QName of the key that this constraint refers to.
	 */
	var Refer(default, default):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
