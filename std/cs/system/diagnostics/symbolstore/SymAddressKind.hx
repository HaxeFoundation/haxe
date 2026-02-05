package cs.system.diagnostics.symbolstore;

/** Specifies address types for local variables, parameters, and fields in the methods , , and  of the  interface. */
@:native("System.Diagnostics.SymbolStore.SymAddressKind")
extern enum abstract SymAddressKind(Int) {
	var BitField = 9;
	var ILOffset = 1;
	var NativeOffset = 5;
	var NativeRegister = 3;
	var NativeRegisterRegister = 6;
	var NativeRegisterRelative = 4;
	var NativeRegisterStack = 7;
	var NativeRVA = 2;
	var NativeSectionOffset = 10;
	var NativeStackRegister = 8;
}
