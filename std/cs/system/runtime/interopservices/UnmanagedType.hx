package cs.system.runtime.interopservices;

/** Identifies how to marshal parameters or fields to unmanaged code. */
@:native("System.Runtime.InteropServices.UnmanagedType")
extern enum UnmanagedType {
	AnsiBStr;
	AsAny;
	Bool;
	BStr;
	ByValArray;
	ByValTStr;
	Currency;
	CustomMarshaler;
	Error;
	FunctionPtr;
	HString;
	I1;
	I2;
	I4;
	I8;
	IDispatch;
	IInspectable;
	Interface;
	IUnknown;
	LPArray;
	LPStr;
	LPStruct;
	LPTStr;
	LPUTF8Str;
	LPWStr;
	R4;
	R8;
	SafeArray;
	Struct;
	SysInt;
	SysUInt;
	TBStr;
	U1;
	U2;
	U4;
	U8;
	VariantBool;
	VBByRefStr;
}
