package cs.system.xml.serialization;

/** Generates mappings to XML schema element declarations, including literal XML Schema Definition (XSD) message parts in a Web Services Description Language (WSDL) document, for .NET Framework types or Web service method information. */
@:native("System.Xml.Serialization.XmlReflectionImporter")
extern class XmlReflectionImporter {
	@:overload(function():Void {})
	@:overload(function(defaultNamespace:String):Void {})
	@:overload(function(attributeOverrides:cs.system.xml.serialization.XmlAttributeOverrides):Void {})
	function new(attributeOverrides:cs.system.xml.serialization.XmlAttributeOverrides, defaultNamespace:String):Void;
	@:overload(function(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>, hasWrapperElement:Bool):cs.system.xml.serialization.XmlMembersMapping {})
	@:overload(function(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>, hasWrapperElement:Bool, rpc:Bool):cs.system.xml.serialization.XmlMembersMapping {})
	@:overload(function(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>, hasWrapperElement:Bool, rpc:Bool, openModel:Bool):cs.system.xml.serialization.XmlMembersMapping {})
	/**
	 * Generates internal type mappings for information from a Web service method.
	 * @param elementName An XML element name produced from the Web service method.
	 * @param ns An XML element namespace produced from the Web service method.
	 * @param members An array of  objects that contain .NET Framework code entities
	 * that belong to a Web service method.
	 * @param hasWrapperElement if elements that correspond to Web Services Description
	 * Language (WSDL) message parts should be enclosed in an extra wrapper element in
	 * a SOAP message; otherwise, .
	 * @return An  with mappings to the element parts of a WSDL message definition.
	 */
	function ImportMembersMapping(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>, hasWrapperElement:Bool, rpc:Bool, openModel:Bool, access:cs.system.xml.serialization.XmlMappingAccess):cs.system.xml.serialization.XmlMembersMapping;
	@:overload(function(type:cs.system.Type):cs.system.xml.serialization.XmlTypeMapping {})
	@:overload(function(type:cs.system.Type, defaultNamespace:String):cs.system.xml.serialization.XmlTypeMapping {})
	@:overload(function(type:cs.system.Type, root:cs.system.xml.serialization.XmlRootAttribute):cs.system.xml.serialization.XmlTypeMapping {})
	/**
	 * Generates a mapping to an XML Schema element for a specified .NET Framework
	 * type.
	 * @param type The .NET Framework type for which to generate a type mapping.
	 * @return Internal .NET Framework mapping of a type to an XML Schema element.
	 */
	function ImportTypeMapping(type:cs.system.Type, root:cs.system.xml.serialization.XmlRootAttribute, defaultNamespace:String):cs.system.xml.serialization.XmlTypeMapping;
	/**
	 * Includes mappings for a type for later use when import methods are invoked.
	 * @param type The .NET Framework type for which to save type mapping information.
	 */
	function IncludeType(type:cs.system.Type):Void;
	/**
	 * Includes mappings for derived types for later use when import methods are
	 * invoked.
	 * @param provider An instance of the  class that contains custom attributes
	 * derived from the  attribute.
	 */
	function IncludeTypes(provider:cs.system.reflection.ICustomAttributeProvider):Void;
}
