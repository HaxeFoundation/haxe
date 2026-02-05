package cs.system.diagnostics.symbolstore;

/** Represents a lexical scope within , providing access to the start and end offsets of the scope, as well as its child and parent scopes. */
@:native("System.Diagnostics.SymbolStore.ISymbolScope")
extern interface ISymbolScope {
	/**
	 * Gets the end offset of the current lexical scope.
	 * @return The end offset of the current lexical scope.
	 */
	var EndOffset(default, never):Int;
	/**
	 * Gets the method that contains the current lexical scope.
	 * @return The method that contains the current lexical scope.
	 */
	var Method(default, never):cs.system.diagnostics.symbolstore.ISymbolMethod;
	/**
	 * Gets the parent lexical scope of the current scope.
	 * @return The parent lexical scope of the current scope.
	 */
	var Parent(default, never):cs.system.diagnostics.symbolstore.ISymbolScope;
	/**
	 * Gets the start offset of the current lexical scope.
	 * @return The start offset of the current lexical scope.
	 */
	var StartOffset(default, never):Int;
	/**
	 * Gets the child lexical scopes of the current lexical scope.
	 * @return The child lexical scopes that of the current lexical scope.
	 */
	function GetChildren():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolScope>;
	/**
	 * Gets the local variables within the current lexical scope.
	 * @return The local variables within the current lexical scope.
	 */
	function GetLocals():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolVariable>;
	/**
	 * Gets the namespaces that are used within the current scope.
	 * @return The namespaces that are used within the current scope.
	 */
	function GetNamespaces():cs.NativeArray<cs.system.diagnostics.symbolstore.ISymbolNamespace>;
}
