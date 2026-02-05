package cs.system.runtime.serialization;

/** Allows the transformation of a set of .NET Framework types that are used in data contracts into an XML schema file (.xsd). */
@:native("System.Runtime.Serialization.XsdDataContractExporter")
extern class XsdDataContractExporter {
	/**
	 * Gets or sets an  that contains options that can be set for the export operation.
	 * @return An  that contains options used to customize how types are exported to
	 * schemas.
	 */
	var Options(default, default):cs.system.runtime.serialization.ExportOptions;
	/**
	 * Gets the collection of exported XML schemas.
	 * @return An  that contains the schemas transformed from the set of common
	 * language runtime (CLR) types after calling the  method.
	 */
	var Schemas(default, never):cs.system.xml.schema.XmlSchemaSet;
	@:overload(function():Void {})
	function new(schemas:cs.system.xml.schema.XmlSchemaSet):Void;
	@:overload(function(assemblies:cs.system.collections.generic.ICollection<cs.system.reflection.Assembly>):Bool {})
	@:overload(function(types:cs.system.collections.generic.ICollection<cs.system.Type>):Bool {})
	/**
	 * Gets a value that indicates whether the set of .common language runtime (CLR)
	 * types contained in a set of assemblies can be exported.
	 * @param assemblies A  of  that contains the assemblies with the types to export.
	 * @return if the types can be exported; otherwise, .
	 */
	function CanExport(type:cs.system.Type):Bool;
	@:overload(function(assemblies:cs.system.collections.generic.ICollection<cs.system.reflection.Assembly>):Void {})
	@:overload(function(types:cs.system.collections.generic.ICollection<cs.system.Type>):Void {})
	/**
	 * Transforms the types contained in the specified collection of assemblies.
	 * @param assemblies A  (of ) that contains the types to export.
	 */
	function Export(type:cs.system.Type):Void;
	/**
	 * Returns the top-level name and namespace for the .
	 * @param type The  to query.
	 * @return The  that represents the top-level name and namespace for this , which
	 * is written to the stream when writing this object.
	 */
	function GetRootElementName(type:cs.system.Type):cs.system.xml.XmlQualifiedName;
	/**
	 * Returns the XML schema type for the specified type.
	 * @param type The type to return a schema for.
	 * @return An  that contains the XML schema.
	 */
	function GetSchemaType(type:cs.system.Type):cs.system.xml.schema.XmlSchemaType;
	/**
	 * Returns the contract name and contract namespace for the .
	 * @param type The  that was exported.
	 * @return An  that represents the contract name of the type and its namespace.
	 */
	function GetSchemaTypeName(type:cs.system.Type):cs.system.xml.XmlQualifiedName;
}
