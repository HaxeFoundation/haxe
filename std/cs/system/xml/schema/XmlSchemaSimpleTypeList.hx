package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class can be used to define a  element as a list of values of a specified data type. */
@:native("System.Xml.Schema.XmlSchemaSimpleTypeList")
extern class XmlSchemaSimpleTypeList extends cs.system.xml.schema.XmlSchemaSimpleTypeContent {
	/**
	 * Gets or sets the  representing the type of the  element based on the  and 
	 * values of the simple type.
	 * @return The  representing the type of the  element.
	 */
	var BaseItemType(default, default):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets or sets the  element that is derived from the type specified by the base
	 * value.
	 * @return The item type for the simple type element.
	 */
	var ItemType(default, default):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Gets or sets the name of a built-in data type or  element defined in this schema
	 * (or another schema indicated by the specified namespace).
	 * @return The type name of the simple type list.
	 */
	var ItemTypeName(default, default):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
