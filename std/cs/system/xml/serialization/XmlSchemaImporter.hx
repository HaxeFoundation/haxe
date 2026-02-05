package cs.system.xml.serialization;

/** Generates internal mappings to .NET Framework types for XML schema element declarations, including literal XSD message parts in a WSDL document. */
@:native("System.Xml.Serialization.XmlSchemaImporter")
extern class XmlSchemaImporter extends cs.system.xml.serialization.SchemaImporter {
	@:overload(function(schemas:cs.system.xml.serialization.XmlSchemas):Void {})
	function new(schemas:cs.system.xml.serialization.XmlSchemas, typeIdentifiers:cs.system.xml.serialization.CodeIdentifiers):Void;
	/**
	 * Generates internal type mapping information for a single, (SOAP) literal element
	 * part defined in a WSDL document.
	 * @param typeName An  that specifies the name of an element's type for which a
	 * .NET Framework type is generated.
	 * @param elementName The name of the part element in the WSDL document.
	 * @return An  representing the .NET Framework type mapping for a single element
	 * part of a WSDL message definition.
	 */
	function ImportAnyType(typeName:cs.system.xml.XmlQualifiedName, elementName:String):cs.system.xml.serialization.XmlMembersMapping;
	@:overload(function(name:cs.system.xml.XmlQualifiedName, baseType:cs.system.Type):cs.system.xml.serialization.XmlTypeMapping {})
	/**
	 * Generates internal type mapping information for an element defined in an XML
	 * schema document.
	 * @param name An  that specifies the name of an element defined in an XML schema
	 * document.
	 * @param baseType A base type for the .NET Framework type that is generated to
	 * correspond to an XSD element's type.
	 * @return An  representing the.NET Framework type mapping information for an XML
	 * schema element.
	 */
	function ImportDerivedTypeMapping(name:cs.system.xml.XmlQualifiedName, baseType:cs.system.Type, baseTypeCanBeIndirect:Bool):cs.system.xml.serialization.XmlTypeMapping;
	@:overload(function(name:cs.system.xml.XmlQualifiedName):cs.system.xml.serialization.XmlMembersMapping {})
	@:overload(function(names:cs.NativeArray<cs.system.xml.XmlQualifiedName>):cs.system.xml.serialization.XmlMembersMapping {})
	@:overload(function(name:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.SoapSchemaMember>):cs.system.xml.serialization.XmlMembersMapping {})
	/**
	 * Generates internal type mapping information for the element parts of a
	 * literal-use SOAP message defined in a WSDL document.
	 * @param name The name of the element for which to generate a mapping.
	 * @param ns The namespace of the element for which to generate a mapping.
	 * @param members An array of  instances that specifies the members of the element
	 * for which to generate a mapping.
	 * @return A  that contains type mapping information.
	 */
	function ImportMembersMapping(names:cs.NativeArray<cs.system.xml.XmlQualifiedName>, baseType:cs.system.Type, baseTypeCanBeIndirect:Bool):cs.system.xml.serialization.XmlMembersMapping;
	@:overload(function(typeName:cs.system.xml.XmlQualifiedName):cs.system.xml.serialization.XmlTypeMapping {})
	@:overload(function(typeName:cs.system.xml.XmlQualifiedName, baseType:cs.system.Type):cs.system.xml.serialization.XmlTypeMapping {})
	/**
	 * Generates internal type mapping information for an element defined in an XML
	 * schema document.
	 * @param typeName A  that specifies an XML element.
	 * @return A  object that describes a type mapping.
	 */
	function ImportSchemaType(typeName:cs.system.xml.XmlQualifiedName, baseType:cs.system.Type, baseTypeCanBeIndirect:Bool):cs.system.xml.serialization.XmlTypeMapping;
	/**
	 * Generates internal type mapping information for an element defined in an XML
	 * schema document.
	 * @param name An  that specifies the name of an element defined in an XML schema
	 * document.
	 * @return The .NET Framework type mapping information for an XML schema element.
	 */
	function ImportTypeMapping(name:cs.system.xml.XmlQualifiedName):cs.system.xml.serialization.XmlTypeMapping;
}
