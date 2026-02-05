package cs.system.diagnostics.symbolstore;

/** Represents a symbol reader for managed code. */
@:native("System.Diagnostics.SymbolStore.ISymbolReader")
extern interface ISymbolReader {
	/**
	 * Gets the metadata token for the method that was specified as the user entry
	 * point for the module, if any.
	 * @return The metadata token for the method that is the user entry point for the
	 * module.
	 */
	var UserEntryPoint(default, never):cs.system.diagnostics.symbolstore.SymbolToken;
	/**
	 * Gets a document specified by the language, vendor, and type.
	 * @param url The URL that identifies the document.
	 * @param language The document language. You can specify this parameter as .
	 * @param languageVendor The identity of the vendor for the document language. You
	 * can specify this parameter as .
	 * @param documentType The type of the document. You can specify this parameter as
	 * .
	 * @return The specified document.
	 */
	function GetDocument(url:String, language:cs.system.Guid, languageVendor:cs.system.Guid, documentType:cs.system.Guid):cs.system.diagnostics.symbolstore.ISymbolDocument;
	/**
	 * Gets an array of all documents defined in the symbol store.
	 * @return An array of all documents defined in the symbol store.
	 */
	function GetDocuments():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolDocument>;
	/**
	 * Gets all global variables in the module.
	 * @return An array of all variables in the module.
	 */
	function GetGlobalVariables():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolVariable>;
	@:overload(function(method:cs.system.diagnostics.symbolstore.SymbolToken):cs.system.diagnostics.symbolstore.ISymbolMethod {})
	/**
	 * Gets a symbol reader method object when given the identifier of a method.
	 * @param method The metadata token of the method.
	 * @return The symbol reader method object for the specified method identifier.
	 */
	function GetMethod(method:cs.system.diagnostics.symbolstore.SymbolToken, version:Int):cs.system.diagnostics.symbolstore.ISymbolMethod;
	/**
	 * Gets a symbol reader method object that contains a specified position in a
	 * document.
	 * @param document The document in which the method is located.
	 * @param line The position of the line within the document. The lines are
	 * numbered, beginning with 1.
	 * @param column The position of column within the document. The columns are
	 * numbered, beginning with 1.
	 * @return The reader method object for the specified position in the document.
	 */
	function GetMethodFromDocumentPosition(document:cs.system.diagnostics.symbolstore.ISymbolDocument, line:Int, column:Int):cs.system.diagnostics.symbolstore.ISymbolMethod;
	/**
	 * Gets the namespaces that are defined in the global scope within the current
	 * symbol store.
	 * @return The namespaces defined in the global scope within the current symbol
	 * store.
	 */
	function GetNamespaces():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolNamespace>;
	/**
	 * Gets an attribute value when given the attribute name.
	 * @param parent The metadata token for the object for which the attribute is
	 * requested.
	 * @param name The attribute name.
	 * @return The value of the attribute.
	 */
	function GetSymAttribute(parent:cs.system.diagnostics.symbolstore.SymbolToken, name:String):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the variables that are not local when given the parent.
	 * @param parent The metadata token for the type for which the variables are
	 * requested.
	 * @return An array of variables for the parent.
	 */
	function GetVariables(parent:cs.system.diagnostics.symbolstore.SymbolToken):cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolVariable>;
}
