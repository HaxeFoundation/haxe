package cs.system.xml.serialization;

/** Provides mappings between .NET Framework Web service methods and Web Services Description Language (WSDL) messages that are defined for SOAP Web services. */
@:native("System.Xml.Serialization.XmlMembersMapping")
extern class XmlMembersMapping extends cs.system.xml.serialization.XmlMapping {
	/**
	 * Gets the number of .NET Framework code entities that belong to a Web service
	 * method to which a SOAP message is being mapped.
	 * @return The number of mappings in the collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets the name of the .NET Framework type being mapped to the data type of an XML
	 * Schema element that represents a SOAP message.
	 * @return The name of the .NET Framework type.
	 */
	var TypeName(default, never):String;
	/**
	 * Gets the namespace of the .NET Framework type being mapped to the data type of
	 * an XML Schema element that represents a SOAP message.
	 * @return The .NET Framework namespace of the mapping.
	 */
	var TypeNamespace(default, never):String;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.xml.serialization.XmlMemberMapping;
}
