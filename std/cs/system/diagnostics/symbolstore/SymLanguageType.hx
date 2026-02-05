package cs.system.diagnostics.symbolstore;

/** Holds the public GUIDs for language types to be used with the symbol store. */
@:native("System.Diagnostics.SymbolStore.SymLanguageType")
extern class SymLanguageType {
	/** Specifies the GUID of the Basic language type to be used with the symbol store. */
	static var Basic(default, never):cs.system.Guid;
	/** Specifies the GUID of the C language type to be used with the symbol store. */
	static var C(default, never):cs.system.Guid;
	/** Specifies the GUID of the Cobol language type to be used with the symbol store. */
	static var Cobol(default, never):cs.system.Guid;
	/** Specifies the GUID of the C++ language type to be used with the symbol store. */
	static var CPlusPlus(default, never):cs.system.Guid;
	/** Specifies the GUID of the C# language type to be used with the symbol store. */
	static var CSharp(default, never):cs.system.Guid;
	/** Specifies the GUID of the ILAssembly language type to be used with the symbol store. */
	static var ILAssembly(default, never):cs.system.Guid;
	/** Specifies the GUID of the Java language type to be used with the symbol store. */
	static var Java(default, never):cs.system.Guid;
	/** Specifies the GUID of the JScript language type to be used with the symbol store. */
	static var JScript(default, never):cs.system.Guid;
	/** Specifies the GUID of the C++ language type to be used with the symbol store. */
	static var MCPlusPlus(default, never):cs.system.Guid;
	/** Specifies the GUID of the Pascal language type to be used with the symbol store. */
	static var Pascal(default, never):cs.system.Guid;
	/** Specifies the GUID of the SMC language type to be used with the symbol store. */
	static var SMC(default, never):cs.system.Guid;
	function new():Void;
}
