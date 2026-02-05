package cs.system.xml.serialization;

/** Populates  objects with XML schema element declarations that are found in type mapping objects. */
@:native("System.Xml.Serialization.XmlSchemaExporter")
extern class XmlSchemaExporter {
	function new(schemas:cs.system.xml.serialization.XmlSchemas):Void;
	@:overload(function(ns:String):String {})
	/**
	 * Exports an <any> element to the  object that is identified by the specified
	 * namespace.
	 * @param ns The namespace of the XML schema document to which to add an <any>
	 * element.
	 * @return An arbitrary name assigned to the <any> element declaration.
	 */
	function ExportAnyType(members:cs.system.xml.serialization.XmlMembersMapping):String;
	@:overload(function(xmlMembersMapping:cs.system.xml.serialization.XmlMembersMapping):Void {})
	/**
	 * Adds an element declaration to the applicable  for each of the element parts of
	 * a literal SOAP message definition.
	 * @param xmlMembersMapping The internal .NET Framework type mappings for the
	 * element parts of a Web Services Description Language (WSDL) message definition.
	 */
	function ExportMembersMapping(xmlMembersMapping:cs.system.xml.serialization.XmlMembersMapping, exportEnclosingType:Bool):Void;
	@:overload(function(xmlMembersMapping:cs.system.xml.serialization.XmlMembersMapping):cs.system.xml.XmlQualifiedName {})
	/**
	 * Adds an element declaration to the applicable  object for a single element part
	 * of a literal SOAP message definition.
	 * @param xmlMembersMapping Internal .NET Framework type mappings for the element
	 * parts of a Web Services Description Language (WSDL) message definition.
	 * @return An  that represents the qualified XML name of the exported element
	 * declaration.
	 */
	function ExportTypeMapping(xmlTypeMapping:cs.system.xml.serialization.XmlTypeMapping):Void;
}
