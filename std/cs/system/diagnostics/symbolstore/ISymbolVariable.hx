package cs.system.diagnostics.symbolstore;

/** Represents a variable within a symbol store. */
@:native("System.Diagnostics.SymbolStore.ISymbolVariable")
extern interface ISymbolVariable {
	/**
	 * Gets the first address of a variable.
	 * @return The first address of the variable.
	 */
	var AddressField1(default, never):Int;
	/**
	 * Gets the second address of a variable.
	 * @return The second address of the variable.
	 */
	var AddressField2(default, never):Int;
	/**
	 * Gets the third address of a variable.
	 * @return The third address of the variable.
	 */
	var AddressField3(default, never):Int;
	/**
	 * Gets the  value describing the type of the address.
	 * @return The type of the address. One of the  values.
	 */
	var AddressKind(default, never):cs.system.diagnostics.symbolstore.SymAddressKind;
	/**
	 * Gets the attributes of the variable.
	 * @return The variable attributes.
	 */
	var Attributes(default, never):Dynamic;
	/**
	 * Gets the end offset of a variable within the scope of the variable.
	 * @return The end offset of the variable.
	 */
	var EndOffset(default, never):Int;
	/**
	 * Gets the name of the variable.
	 * @return The name of the variable.
	 */
	var Name(default, never):String;
	/**
	 * Gets the start offset of the variable within the scope of the variable.
	 * @return The start offset of the variable.
	 */
	var StartOffset(default, never):Int;
	/**
	 * Gets the variable signature.
	 * @return The variable signature as an opaque blob.
	 */
	function GetSignature():cs.NativeArray<cs.UInt8>;
}
