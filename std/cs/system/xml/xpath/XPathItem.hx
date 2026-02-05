package cs.system.xml.xpath;

/** Represents an item in the XQuery 1.0 and XPath 2.0 Data Model. */
@:native("System.Xml.XPath.XPathItem")
extern class XPathItem {
	/**
	 * When overridden in a derived class, gets a value indicating whether the item
	 * represents an XPath node or an atomic value.
	 * @return if the item represents an XPath node;  if the item represents an atomic
	 * value.
	 */
	var IsNode(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the current item as a boxed object of
	 * the most appropriate .NET Framework 2.0 type according to its schema type.
	 * @return The current item as a boxed object of the most appropriate .NET
	 * Framework type.
	 */
	var TypedValue(default, never):Dynamic;
	/**
	 * When overridden in a derived class, gets the  value of the item.
	 * @return The  value of the item.
	 */
	var Value(default, never):String;
	/**
	 * When overridden in a derived class, gets the item's value as a .
	 * @return The item's value as a .
	 */
	var ValueAsBoolean(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the item's value as a .
	 * @return The item's value as a .
	 */
	var ValueAsDateTime(default, never):cs.system.DateTime;
	/**
	 * When overridden in a derived class, gets the item's value as a .
	 * @return The item's value as a .
	 */
	var ValueAsDouble(default, never):Float;
	/**
	 * When overridden in a derived class, gets the item's value as an .
	 * @return The item's value as an .
	 */
	var ValueAsInt(default, never):Int;
	/**
	 * When overridden in a derived class, gets the item's value as an .
	 * @return The item's value as an .
	 */
	var ValueAsLong(default, never):haxe.Int64;
	/**
	 * When overridden in a derived class, gets the .NET Framework 2.0 type of the
	 * item.
	 * @return The .NET Framework type of the item. The default value is .
	 */
	var ValueType(default, never):cs.system.Type;
	/**
	 * When overridden in a derived class, gets the  for the item.
	 * @return The  for the item.
	 */
	var XmlType(default, never):cs.system.xml.schema.XmlSchemaType;
	@:overload(function(returnType:cs.system.Type):Dynamic {})
	/**
	 * Returns the item's value as the specified type.
	 * @param returnType The type to return the item value as.
	 * @return The value of the item as the type requested.
	 */
	function ValueAs(returnType:cs.system.Type, nsResolver:cs.system.xml.IXmlNamespaceResolver):Dynamic;
}
