package cs.system.runtime.interopservices;

/** Identifies how to marshal parameters or fields to unmanaged code. */
@:native("System.Runtime.InteropServices.UnmanagedType")
extern enum abstract UnmanagedType(Int) {
	var AnsiBStr = 35;
	var AsAny = 40;
	var Bool = 2;
	var BStr = 19;
	var ByValArray = 30;
	var ByValTStr = 23;
	var Currency = 15;
	var CustomMarshaler = 44;
	var Error = 45;
	var FunctionPtr = 38;
	var HString = 47;
	var I1 = 3;
	var I2 = 5;
	var I4 = 7;
	var I8 = 9;
	var IDispatch = 26;
	var IInspectable = 46;
	var Interface = 28;
	var IUnknown = 25;
	var LPArray = 42;
	var LPStr = 20;
	var LPStruct = 43;
	var LPTStr = 22;
	var LPUTF8Str = 48;
	var LPWStr = 21;
	var R4 = 11;
	var R8 = 12;
	var SafeArray = 29;
	var Struct = 27;
	var SysInt = 31;
	var SysUInt = 32;
	var TBStr = 36;
	var U1 = 4;
	var U2 = 6;
	var U4 = 8;
	var U8 = 10;
	var VariantBool = 37;
	var VBByRefStr = 34;
}
