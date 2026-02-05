package cs.system.diagnostics.symbolstore;

/** Represents a document referenced by a symbol store. */
@:native("System.Diagnostics.SymbolStore.ISymbolDocument")
extern interface ISymbolDocument {
	/**
	 * Gets the checksum algorithm identifier.
	 * @return A GUID identifying the checksum algorithm. The value is all zeros, if
	 * there is no checksum.
	 */
	var CheckSumAlgorithmId(default, never):cs.system.Guid;
	/**
	 * Gets the type of the current document.
	 * @return The type of the current document.
	 */
	var DocumentType(default, never):cs.system.Guid;
	/**
	 * Checks whether the current document is stored in the symbol store.
	 * @return if the current document is stored in the symbol store; otherwise, .
	 */
	var HasEmbeddedSource(default, never):Bool;
	/**
	 * Gets the language of the current document.
	 * @return The language of the current document.
	 */
	var Language(default, never):cs.system.Guid;
	/**
	 * Gets the language vendor of the current document.
	 * @return The language vendor of the current document.
	 */
	var LanguageVendor(default, never):cs.system.Guid;
	/**
	 * Gets the length, in bytes, of the embedded source.
	 * @return The source length of the current document.
	 */
	var SourceLength(default, never):Int;
	/**
	 * Gets the URL of the current document.
	 * @return The URL of the current document.
	 */
	var URL(default, never):String;
	/**
	 * Returns the closest line that is a sequence point, given a line in the current
	 * document that might or might not be a sequence point.
	 * @param line The specified line in the document.
	 * @return The closest line that is a sequence point.
	 */
	function FindClosestLine(line:Int):Int;
	/**
	 * Gets the checksum.
	 * @return The checksum.
	 */
	function GetCheckSum():cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the embedded document source for the specified range.
	 * @param startLine The starting line in the current document.
	 * @param startColumn The starting column in the current document.
	 * @param endLine The ending line in the current document.
	 * @param endColumn The ending column in the current document.
	 * @return The document source for the specified range.
	 */
	function GetSourceRange(startLine:Int, startColumn:Int, endLine:Int, endColumn:Int):cs.NativeArray<cs.UInt8>;
}
