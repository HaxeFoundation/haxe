package cs.system.xml.serialization;

/** Generates mappings to SOAP-encoded messages from .NET Framework types or Web service method information. */
@:native("System.Xml.Serialization.SoapReflectionImporter")
extern class SoapReflectionImporter {
	@:overload(function():Void {})
	@:overload(function(defaultNamespace:String):Void {})
	@:overload(function(attributeOverrides:cs.system.xml.serialization.SoapAttributeOverrides):Void {})
	function new(attributeOverrides:cs.system.xml.serialization.SoapAttributeOverrides, defaultNamespace:String):Void;
	@:overload(function(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>):cs.system.xml.serialization.XmlMembersMapping {})
	@:overload(function(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>, hasWrapperElement:Bool, writeAccessors:Bool):cs.system.xml.serialization.XmlMembersMapping {})
	@:overload(function(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>, hasWrapperElement:Bool, writeAccessors:Bool, validate:Bool):cs.system.xml.serialization.XmlMembersMapping {})
	/**
	 * Generates internal type mappings for information that is gathered from a Web
	 * service method.
	 * @param elementName An XML element name produced from the Web service method.
	 * @param ns An XML element namespace produced from the Web service method.
	 * @param members An array of .NET Framework code entities that belong to a Web
	 * service method.
	 * @return Internal .NET Framework type mappings to the element parts of a WSDL
	 * message definition.
	 */
	function ImportMembersMapping(elementName:String, ns:String, members:cs.NativeArray<cs.system.xml.serialization.XmlReflectionMember>, hasWrapperElement:Bool, writeAccessors:Bool, validate:Bool, access:cs.system.xml.serialization.XmlMappingAccess):cs.system.xml.serialization.XmlMembersMapping;
	@:overload(function(type:cs.system.Type):cs.system.xml.serialization.XmlTypeMapping {})
	/**
	 * Generates a mapping to an XML Schema element for a .NET Framework type.
	 * @param type The .NET Framework type for which to generate a type mapping.
	 * @return Internal .NET Framework mapping of a type to an XML Schema element.
	 */
	function ImportTypeMapping(type:cs.system.Type, defaultNamespace:String):cs.system.xml.serialization.XmlTypeMapping;
	/**
	 * Places mappings for a type in the  instance's context for later use when import
	 * methods are invoked.
	 * @param type The .NET Framework type for which to save type mapping information.
	 */
	function IncludeType(type:cs.system.Type):Void;
	/**
	 * Places mappings for derived types in the  instance's context for later use when
	 * import methods are invoked.
	 * @param provider An  reflection object that contains custom attributes that are
	 * derived from the  attribute.
	 */
	function IncludeTypes(provider:cs.system.reflection.ICustomAttributeProvider):Void;
}
