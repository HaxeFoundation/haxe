package cs.system.runtime.interopservices.comtypes;

/** Contains information about how to transfer a structure element, parameter, or function return value between processes. */
@:native("System.Runtime.InteropServices.ComTypes.PARAMDESC")
extern class PARAMDESC extends cs.system.ValueType {
	/** Represents a pointer to a value that is being passed between processes. */
	var lpVarValue:cs.system.IntPtr;
	/** Represents bitmask values that describe the structure element, parameter, or return value. */
	var wParamFlags:cs.system.runtime.interopservices.comtypes.PARAMFLAG;
}
