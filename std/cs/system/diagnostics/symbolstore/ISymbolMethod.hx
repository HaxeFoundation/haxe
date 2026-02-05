package cs.system.diagnostics.symbolstore;

/** Represents a method within a symbol store. */
@:native("System.Diagnostics.SymbolStore.ISymbolMethod")
extern interface ISymbolMethod {
	/**
	 * Gets the root lexical scope for the current method. This scope encloses the
	 * entire method.
	 * @return The root lexical scope that encloses the entire method.
	 */
	var RootScope(default, never):cs.system.diagnostics.symbolstore.ISymbolScope;
	/**
	 * Gets a count of the sequence points in the method.
	 * @return The count of the sequence points in the method.
	 */
	var SequencePointCount(default, never):Int;
	/**
	 * Gets the  containing the metadata for the current method.
	 * @return The metadata token for the current method.
	 */
	var Token(default, never):cs.system.diagnostics.symbolstore.SymbolToken;
	/**
	 * Gets the namespace that the current method is defined within.
	 * @return The namespace that the current method is defined within.
	 */
	function GetNamespace():cs.system.diagnostics.symbolstore.ISymbolNamespace;
	/**
	 * Gets the Microsoft intermediate language (MSIL) offset within the method that
	 * corresponds to the specified position.
	 * @param document The document for which the offset is requested.
	 * @param line The document line corresponding to the offset.
	 * @param column The document column corresponding to the offset.
	 * @return The offset within the specified document.
	 */
	function GetOffset(document:cs.system.diagnostics.symbolstore.ISymbolDocument, line:Int, column:Int):Int;
	/**
	 * Gets the parameters for the current method.
	 * @return The array of parameters for the current method.
	 */
	function GetParameters():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolVariable>;
	/**
	 * Gets an array of start and end offset pairs that correspond to the ranges of
	 * Microsoft intermediate language (MSIL) that a given position covers within this
	 * method.
	 * @param document The document for which the offset is requested.
	 * @param line The document line corresponding to the ranges.
	 * @param column The document column corresponding to the ranges.
	 * @return An array of start and end offset pairs.
	 */
	function GetRanges(document:cs.system.diagnostics.symbolstore.ISymbolDocument, line:Int, column:Int):cs.NativeArray<Int>;
	/**
	 * Returns the most enclosing lexical scope when given an offset within a method.
	 * @param offset The byte offset within the method of the lexical scope.
	 * @return The most enclosing lexical scope for the given byte offset within the
	 * method.
	 */
	function GetScope(offset:Int):cs.system.diagnostics.symbolstore.ISymbolScope;
	/**
	 * Gets the sequence points for the current method.
	 * @param offsets The array of byte offsets from the beginning of the method for
	 * the sequence points.
	 * @param documents The array of documents in which the sequence points are
	 * located.
	 * @param lines The array of lines in the documents at which the sequence points
	 * are located.
	 * @param columns The array of columns in the documents at which the sequence
	 * points are located.
	 * @param endLines The array of lines in the documents at which the sequence points
	 * end.
	 * @param endColumns The array of columns in the documents at which the sequence
	 * points end.
	 */
	function GetSequencePoints(offsets:cs.NativeArray<Int>, documents:cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolDocument>, lines:cs.NativeArray<Int>, columns:cs.NativeArray<Int>, endLines:cs.NativeArray<Int>, endColumns:cs.NativeArray<Int>):Void;
	/**
	 * Gets the start and end positions for the source of the current method.
	 * @param docs The starting and ending source documents.
	 * @param lines The starting and ending lines in the corresponding source
	 * documents.
	 * @param columns The starting and ending columns in the corresponding source
	 * documents.
	 * @return if the positions were defined; otherwise, .
	 */
	function GetSourceStartEnd(docs:cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolDocument>, lines:cs.NativeArray<Int>, columns:cs.NativeArray<Int>):Bool;
}
