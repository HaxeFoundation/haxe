package cs.system.diagnostics.symbolstore;

/** Represents a namespace within a symbol store. */
@:native("System.Diagnostics.SymbolStore.ISymbolNamespace")
extern interface ISymbolNamespace {
	/**
	 * Gets the current namespace.
	 * @return The current namespace.
	 */
	var Name(default, never):String;
	/**
	 * Gets the child members of the current namespace.
	 * @return The child members of the current namespace.
	 */
	function GetNamespaces():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolNamespace>;
	/**
	 * Gets all the variables defined at global scope within the current namespace.
	 * @return The variables defined at global scope within the current namespace.
	 */
	function GetVariables():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolVariable>;
}
