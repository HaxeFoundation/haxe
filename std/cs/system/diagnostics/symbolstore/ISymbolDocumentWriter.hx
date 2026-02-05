package cs.system.diagnostics.symbolstore;

/** Represents a document referenced by a symbol store. */
@:native("System.Diagnostics.SymbolStore.ISymbolDocumentWriter")
extern interface ISymbolDocumentWriter {
	/**
	 * Sets checksum information.
	 * @param algorithmId The GUID representing the algorithm ID.
	 * @param checkSum The checksum.
	 */
	function SetCheckSum(algorithmId:cs.system.Guid, checkSum:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Stores the raw source for a document in the symbol store.
	 * @param source The document source represented as unsigned bytes.
	 */
	function SetSource(source:cs.NativeArray<cs.UInt8>):Void;
}
