package cs.system.diagnostics.symbolstore;

/** Specifies address types for local variables, parameters, and fields in the methods , , and  of the  interface. */
@:native("System.Diagnostics.SymbolStore.SymAddressKind")
extern enum SymAddressKind {
	BitField;
	ILOffset;
	NativeOffset;
	NativeRegister;
	NativeRegisterRegister;
	NativeRegisterRelative;
	NativeRegisterStack;
	NativeRVA;
	NativeSectionOffset;
	NativeStackRegister;
}
