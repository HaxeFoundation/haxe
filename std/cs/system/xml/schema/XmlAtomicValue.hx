package cs.system.xml.schema;

/** Represents the typed value of a validated XML element or attribute. The  class cannot be inherited. */
@:native("System.Xml.Schema.XmlAtomicValue")
extern class XmlAtomicValue extends cs.system.xml.xpath.XPathItem {
	/**
	 * Returns a copy of this  object.
	 * @return An  object copy of this  object.
	 */
	function Clone():cs.system.xml.schema.XmlAtomicValue;
	/**
	 * Gets the  value of the validated XML element or attribute.
	 * @return The  value of the validated XML element or attribute.
	 */
	function ToString():String;
	/**
	 * Returns the validated XML element or attribute's value as the type specified
	 * using the  object specified to resolve namespace prefixes.
	 * @param type The type to return the validated XML element or attribute's value
	 * as.
	 * @param nsResolver The  object used to resolve namespace prefixes.
	 * @return The value of the validated XML element or attribute as the type
	 * requested.
	 */
	function ValueAs(type:cs.system.Type, nsResolver:cs.system.xml.IXmlNamespaceResolver):Dynamic;
}
