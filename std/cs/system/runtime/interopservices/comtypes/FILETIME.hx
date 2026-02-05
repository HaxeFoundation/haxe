package cs.system.runtime.interopservices.comtypes;

/** Represents the number of 100-nanosecond intervals since January 1, 1601. This structure is a 64-bit value. */
@:native("System.Runtime.InteropServices.ComTypes.FILETIME")
extern class FILETIME extends cs.system.ValueType {
	/** Specifies the high 32 bits of the . */
	var dwHighDateTime:Int;
	/** Specifies the low 32 bits of the . */
	var dwLowDateTime:Int;
}
