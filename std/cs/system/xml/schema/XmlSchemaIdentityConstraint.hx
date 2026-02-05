package cs.system.xml.schema;

/** Class for the identity constraints: , , and  elements. */
@:native("System.Xml.Schema.XmlSchemaIdentityConstraint")
extern class XmlSchemaIdentityConstraint extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets the collection of fields that apply as children for the XML Path Language
	 * (XPath) expression selector.
	 * @return The collection of fields.
	 */
	var Fields(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the name of the identity constraint.
	 * @return The name of the identity constraint.
	 */
	var Name(default, default):String;
	/**
	 * Gets the qualified name of the identity constraint, which holds the
	 * post-compilation value of the  property.
	 * @return The post-compilation value of the  property.
	 */
	var QualifiedName(default, never):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets the XPath expression  element.
	 * @return The XPath expression  element.
	 */
	var Selector(default, default):cs.system.xml.schema.XmlSchemaXPath;
	function new():Void;
}
