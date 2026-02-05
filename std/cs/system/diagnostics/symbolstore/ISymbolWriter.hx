package cs.system.diagnostics.symbolstore;

/** Represents a symbol writer for managed code. */
@:native("System.Diagnostics.SymbolStore.ISymbolWriter")
extern interface ISymbolWriter {
	/** Closes  and commits the symbols to the symbol store. */
	function Close():Void;
	/** Closes the current method. */
	function CloseMethod():Void;
	/** Closes the most recent namespace. */
	function CloseNamespace():Void;
	/**
	 * Closes the current lexical scope.
	 * @param endOffset The points past the last instruction in the scope.
	 */
	function CloseScope(endOffset:Int):Void;
	/**
	 * Defines a source document.
	 * @param url The URL that identifies the document.
	 * @param language The document language. This parameter can be .
	 * @param languageVendor The identity of the vendor for the document language. This
	 * parameter can be .
	 * @param documentType The type of the document. This parameter can be .
	 * @return The object that represents the document.
	 */
	function DefineDocument(url:String, language:cs.system.Guid, languageVendor:cs.system.Guid, documentType:cs.system.Guid):cs.system.diagnostics.symbolstore.ISymbolDocumentWriter;
	/**
	 * Defines a field in a type or a global field.
	 * @param parent The metadata type or method token.
	 * @param name The field name.
	 * @param attributes A bitwise combination of the field attributes.
	 * @param signature The field signature.
	 * @param addrKind The address types for  and .
	 * @param addr1 The first address for the field specification.
	 * @param addr2 The second address for the field specification.
	 * @param addr3 The third address for the field specification.
	 */
	function DefineField(parent:cs.system.diagnostics.symbolstore.SymbolToken, name:String, attributes:cs.system.reflection.FieldAttributes, signature:cs.NativeArray<cs.UInt8>, addrKind:cs.system.diagnostics.symbolstore.SymAddressKind, addr1:Int, addr2:Int, addr3:Int):Void;
	/**
	 * Defines a single global variable.
	 * @param name The global variable name.
	 * @param attributes A bitwise combination of the global variable attributes.
	 * @param signature The global variable signature.
	 * @param addrKind The address types for , , and .
	 * @param addr1 The first address for the global variable specification.
	 * @param addr2 The second address for the global variable specification.
	 * @param addr3 The third address for the global variable specification.
	 */
	function DefineGlobalVariable(name:String, attributes:cs.system.reflection.FieldAttributes, signature:cs.NativeArray<cs.UInt8>, addrKind:cs.system.diagnostics.symbolstore.SymAddressKind, addr1:Int, addr2:Int, addr3:Int):Void;
	/**
	 * Defines a single variable in the current lexical scope.
	 * @param name The local variable name.
	 * @param attributes A bitwise combination of the local variable attributes.
	 * @param signature The local variable signature.
	 * @param addrKind The address types for , , and .
	 * @param addr1 The first address for the local variable specification.
	 * @param addr2 The second address for the local variable specification.
	 * @param addr3 The third address for the local variable specification.
	 * @param startOffset The start offset for the variable. If this parameter is zero,
	 * it is ignored and the variable is defined throughout the entire scope. If the
	 * parameter is nonzero, the variable falls within the offsets of the current
	 * scope.
	 * @param endOffset The end offset for the variable. If this parameter is zero, it
	 * is ignored and the variable is defined throughout the entire scope. If the
	 * parameter is nonzero, the variable falls within the offsets of the current
	 * scope.
	 */
	function DefineLocalVariable(name:String, attributes:cs.system.reflection.FieldAttributes, signature:cs.NativeArray<cs.UInt8>, addrKind:cs.system.diagnostics.symbolstore.SymAddressKind, addr1:Int, addr2:Int, addr3:Int, startOffset:Int, endOffset:Int):Void;
	/**
	 * Defines a single parameter in the current method. The type of each parameter is
	 * taken from its position within the signature of the method.
	 * @param name The parameter name.
	 * @param attributes A bitwise combination of the parameter attributes.
	 * @param sequence The parameter signature.
	 * @param addrKind The address types for , , and .
	 * @param addr1 The first address for the parameter specification.
	 * @param addr2 The second address for the parameter specification.
	 * @param addr3 The third address for the parameter specification.
	 */
	function DefineParameter(name:String, attributes:cs.system.reflection.ParameterAttributes, sequence:Int, addrKind:cs.system.diagnostics.symbolstore.SymAddressKind, addr1:Int, addr2:Int, addr3:Int):Void;
	/**
	 * Defines a group of sequence points within the current method.
	 * @param document The document object for which the sequence points are being
	 * defined.
	 * @param offsets The sequence point offsets measured from the beginning of
	 * methods.
	 * @param lines The document lines for the sequence points.
	 * @param columns The document positions for the sequence points.
	 * @param endLines The document end lines for the sequence points.
	 * @param endColumns The document end positions for the sequence points.
	 */
	function DefineSequencePoints(document:cs.system.diagnostics.symbolstore.ISymbolDocumentWriter, offsets:cs.NativeArray<Int>, lines:cs.NativeArray<Int>, columns:cs.NativeArray<Int>, endLines:cs.NativeArray<Int>, endColumns:cs.NativeArray<Int>):Void;
	/**
	 * Sets the metadata emitter interface to associate with a writer.
	 * @param emitter The metadata emitter interface.
	 * @param filename The file name for which the debugging symbols are written. Some
	 * writers require a file name, and others do not. If a file name is specified for
	 * a writer that does not use file names, this parameter is ignored.
	 * @param fFullBuild indicates that this is a full rebuild;  indicates that this is
	 * an incremental compilation.
	 */
	function Initialize(emitter:cs.system.IntPtr, filename:String, fFullBuild:Bool):Void;
	/**
	 * Opens a method to place symbol information into.
	 * @param method The metadata token for the method to be opened.
	 */
	function OpenMethod(method:cs.system.diagnostics.symbolstore.SymbolToken):Void;
	/**
	 * Opens a new namespace.
	 * @param name The name of the new namespace.
	 */
	function OpenNamespace(name:String):Void;
	/**
	 * Opens a new lexical scope in the current method.
	 * @param startOffset The offset, in bytes, from the beginning of the method to the
	 * first instruction in the lexical scope.
	 * @return An opaque scope identifier that can be used with  to define the start
	 * and end offsets of a scope at a later time. In this case, the offsets passed to 
	 * and  are ignored. A scope identifier is valid only in the current method.
	 */
	function OpenScope(startOffset:Int):Int;
	/**
	 * Specifies the true start and end of a method within a source file. Use  to
	 * specify the extent of a method, independent of the sequence points that exist
	 * within the method.
	 * @param startDoc The document that contains the starting position.
	 * @param startLine The starting line number.
	 * @param startColumn The starting column.
	 * @param endDoc The document that contains the ending position.
	 * @param endLine The ending line number.
	 * @param endColumn The ending column number.
	 */
	function SetMethodSourceRange(startDoc:cs.system.diagnostics.symbolstore.ISymbolDocumentWriter, startLine:Int, startColumn:Int, endDoc:cs.system.diagnostics.symbolstore.ISymbolDocumentWriter, endLine:Int, endColumn:Int):Void;
	/**
	 * Defines the offset range for the specified lexical scope.
	 * @param scopeID The identifier of the lexical scope.
	 * @param startOffset The byte offset of the beginning of the lexical scope.
	 * @param endOffset The byte offset of the end of the lexical scope.
	 */
	function SetScopeRange(scopeID:Int, startOffset:Int, endOffset:Int):Void;
	/**
	 * Defines an attribute when given the attribute name and the attribute value.
	 * @param parent The metadata token for which the attribute is being defined.
	 * @param name The attribute name.
	 * @param data The attribute value.
	 */
	function SetSymAttribute(parent:cs.system.diagnostics.symbolstore.SymbolToken, name:String, data:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the underlying  (the corresponding unmanaged interface) that a managed 
	 * uses to emit symbols.
	 * @param underlyingWriter A pointer to code that represents the underlying writer.
	 */
	function SetUnderlyingWriter(underlyingWriter:cs.system.IntPtr):Void;
	/**
	 * Identifies the user-defined method as the entry point for the current module.
	 * @param entryMethod The metadata token for the method that is the user entry
	 * point.
	 */
	function SetUserEntryPoint(entryMethod:cs.system.diagnostics.symbolstore.SymbolToken):Void;
	/**
	 * Specifies that the given, fully qualified namespace name is used within the open
	 * lexical scope.
	 * @param fullName The fully qualified name of the namespace.
	 */
	function UsingNamespace(fullName:String):Void;
}
