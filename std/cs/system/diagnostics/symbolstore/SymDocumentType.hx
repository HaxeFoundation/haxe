package cs.system.diagnostics.symbolstore;

/** Holds the public GUIDs for document types to be used with the symbol store. */
@:native("System.Diagnostics.SymbolStore.SymDocumentType")
extern class SymDocumentType {
	/** Specifies the GUID of the document type to be used with the symbol store. */
	static var Text(default, never):cs.system.Guid;
	function new():Void;
}
