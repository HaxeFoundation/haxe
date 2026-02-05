package cs.system.diagnostics.symbolstore;

/** Holds the public GUIDs for language vendors to be used with the symbol store. */
@:native("System.Diagnostics.SymbolStore.SymLanguageVendor")
extern class SymLanguageVendor {
	/** Specifies the GUID of the Microsoft language vendor. */
	static var Microsoft(default, never):cs.system.Guid;
	function new():Void;
}
