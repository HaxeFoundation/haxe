package cs.system.diagnostics.symbolstore;

/** Represents a symbol binder for managed code. */
@:native("System.Diagnostics.SymbolStore.ISymbolBinder1")
extern interface ISymbolBinder1 {
	/**
	 * Gets the interface of the symbol reader for the current file.
	 * @param importer An  that refers to the metadata import interface.
	 * @param filename The name of the file for which the reader interface is required.
	 * @param searchPath The search path used to locate the symbol file.
	 * @return The  interface that reads the debugging symbols.
	 */
	function GetReader(importer:cs.system.IntPtr, filename:String, searchPath:String):cs.system.diagnostics.symbolstore.ISymbolReader;
}
