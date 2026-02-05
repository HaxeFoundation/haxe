package cs.system.xml.serialization;

/** Maps a code entity in a .NET Framework Web service method to an element in a Web Services Description Language (WSDL) message. */
@:native("System.Xml.Serialization.XmlMemberMapping")
extern class XmlMemberMapping {
	/**
	 * Gets or sets a value that indicates whether the .NET Framework type maps to an
	 * XML element or attribute of any type.
	 * @return , if the type maps to an XML any element or attribute; otherwise, .
	 */
	var Any(default, never):Bool;
	/**
	 * Gets a value that indicates whether the accompanying field in the .NET Framework
	 * type has a value specified.
	 * @return , if the accompanying field has a value specified; otherwise, .
	 */
	var CheckSpecified(default, never):Bool;
	/**
	 * Gets the unqualified name of the XML element declaration that applies to this
	 * mapping.
	 * @return The unqualified name of the XML element declaration that applies to this
	 * mapping.
	 */
	var ElementName(default, never):String;
	/**
	 * Gets the name of the Web service method member that is represented by this
	 * mapping.
	 * @return The name of the Web service method member represented by this mapping.
	 */
	var MemberName(default, never):String;
	/**
	 * Gets the XML namespace that applies to this mapping.
	 * @return The XML namespace that applies to this mapping.
	 */
	var Namespace(default, never):String;
	/**
	 * Gets the fully qualified type name of the .NET Framework type for this mapping.
	 * @return The fully qualified type name of the .NET Framework type for this
	 * mapping.
	 */
	var TypeFullName(default, never):String;
	/**
	 * Gets the type name of the .NET Framework type for this mapping.
	 * @return The type name of the .NET Framework type for this mapping.
	 */
	var TypeName(default, never):String;
	/**
	 * Gets the namespace of the .NET Framework type for this mapping.
	 * @return The namespace of the .NET Framework type for this mapping.
	 */
	var TypeNamespace(default, never):String;
	/**
	 * Gets the XML element name as it appears in the service description document.
	 * @return The XML element name.
	 */
	var XsdElementName(default, never):String;
}
