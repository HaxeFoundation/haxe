package cs.system.xml;

/** Provides all the context information required by the  to parse an XML fragment. */
@:native("System.Xml.XmlParserContext")
extern class XmlParserContext {
	/**
	 * Gets or sets the base URI.
	 * @return The base URI to use to resolve the DTD file.
	 */
	var BaseURI(default, default):String;
	/**
	 * Gets or sets the name of the document type declaration.
	 * @return The name of the document type declaration.
	 */
	var DocTypeName(default, default):String;
	/**
	 * Gets or sets the encoding type.
	 * @return An  object indicating the encoding type.
	 */
	var Encoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets the internal DTD subset.
	 * @return The internal DTD subset. For example, this property returns everything
	 * between the square brackets <!DOCTYPE doc [...]>.
	 */
	var InternalSubset(default, default):String;
	/**
	 * Gets or sets the .
	 * @return The .
	 */
	var NamespaceManager(default, default):cs.system.xml.XmlNamespaceManager;
	/**
	 * Gets the  used to atomize strings. For more information on atomized strings, see
	 * .
	 * @return The .
	 */
	var NameTable(default, default):cs.system.xml.XmlNameTable;
	/**
	 * Gets or sets the public identifier.
	 * @return The public identifier.
	 */
	var PublicId(default, default):String;
	/**
	 * Gets or sets the system identifier.
	 * @return The system identifier.
	 */
	var SystemId(default, default):String;
	/**
	 * Gets or sets the current  scope.
	 * @return The current  scope. If there is no  in scope,  is returned.
	 */
	var XmlLang(default, default):String;
	/**
	 * Gets or sets the current  scope.
	 * @return An  value indicating the  scope.
	 */
	var XmlSpace(default, default):cs.system.xml.XmlSpace;
	@:overload(function(nt:cs.system.xml.XmlNameTable, nsMgr:cs.system.xml.XmlNamespaceManager, xmlLang:String, xmlSpace:cs.system.xml.XmlSpace):Void {})
	@:overload(function(nt:cs.system.xml.XmlNameTable, nsMgr:cs.system.xml.XmlNamespaceManager, xmlLang:String, xmlSpace:cs.system.xml.XmlSpace, enc:cs.system.text.Encoding):Void {})
	@:overload(function(nt:cs.system.xml.XmlNameTable, nsMgr:cs.system.xml.XmlNamespaceManager, docTypeName:String, pubId:String, sysId:String, internalSubset:String, baseURI:String, xmlLang:String, xmlSpace:cs.system.xml.XmlSpace):Void {})
	function new(nt:cs.system.xml.XmlNameTable, nsMgr:cs.system.xml.XmlNamespaceManager, docTypeName:String, pubId:String, sysId:String, internalSubset:String, baseURI:String, xmlLang:String, xmlSpace:cs.system.xml.XmlSpace, enc:cs.system.text.Encoding):Void;
}
