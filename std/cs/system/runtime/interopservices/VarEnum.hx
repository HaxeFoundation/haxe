package cs.system.runtime.interopservices;

/** Indicates how to marshal the array elements when an array is marshaled from managed to unmanaged code as a . */
@:native("System.Runtime.InteropServices.VarEnum")
extern enum abstract VarEnum(Int) {
	var VT_ARRAY = 8192;
	var VT_BLOB = 65;
	var VT_BLOB_OBJECT = 70;
	var VT_BOOL = 11;
	var VT_BSTR = 8;
	var VT_BYREF = 16384;
	var VT_CARRAY = 28;
	var VT_CF = 71;
	var VT_CLSID = 72;
	var VT_CY = 6;
	var VT_DATE = 7;
	var VT_DECIMAL = 14;
	var VT_DISPATCH = 9;
	var VT_EMPTY = 0;
	var VT_ERROR = 10;
	var VT_FILETIME = 64;
	var VT_HRESULT = 25;
	var VT_I1 = 16;
	var VT_I2 = 2;
	var VT_I4 = 3;
	var VT_I8 = 20;
	var VT_INT = 22;
	var VT_LPSTR = 30;
	var VT_LPWSTR = 31;
	var VT_NULL = 1;
	var VT_PTR = 26;
	var VT_R4 = 4;
	var VT_R8 = 5;
	var VT_RECORD = 36;
	var VT_SAFEARRAY = 27;
	var VT_STORAGE = 67;
	var VT_STORED_OBJECT = 69;
	var VT_STREAM = 66;
	var VT_STREAMED_OBJECT = 68;
	var VT_UI1 = 17;
	var VT_UI2 = 18;
	var VT_UI4 = 19;
	var VT_UI8 = 21;
	var VT_UINT = 23;
	var VT_UNKNOWN = 13;
	var VT_USERDEFINED = 29;
	var VT_VARIANT = 12;
	var VT_VECTOR = 4096;
	var VT_VOID = 24;
}
