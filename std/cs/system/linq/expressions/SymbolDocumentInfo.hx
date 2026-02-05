package cs.system.linq.expressions;

/** Stores information necessary to emit debugging symbol information for a source file, in particular the file name and unique language identifier. */
@:native("System.Linq.Expressions.SymbolDocumentInfo")
extern class SymbolDocumentInfo {
	/**
	 * Returns the document type's unique identifier, if any. Defaults to the GUID for
	 * a text file.
	 * @return The document type's unique identifier.
	 */
	var DocumentType(default, never):cs.system.Guid;
	/**
	 * The source file name.
	 * @return The string representing the source file name.
	 */
	var FileName(default, never):String;
	/**
	 * Returns the language's unique identifier, if any.
	 * @return The language's unique identifier
	 */
	var Language(default, never):cs.system.Guid;
	/**
	 * Returns the language vendor's unique identifier, if any.
	 * @return The language vendor's unique identifier.
	 */
	var LanguageVendor(default, never):cs.system.Guid;
}
