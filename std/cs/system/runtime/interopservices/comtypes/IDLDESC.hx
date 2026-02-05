package cs.system.runtime.interopservices.comtypes;

/** Contains information needed for transferring a structure element, parameter, or function return value between processes. */
@:native("System.Runtime.InteropServices.ComTypes.IDLDESC")
extern class IDLDESC extends cs.system.ValueType {
	/** Reserved; set to . */
	var dwReserved:cs.system.IntPtr;
	/** Indicates an  value describing the type. */
	var wIDLFlags:cs.system.runtime.interopservices.comtypes.IDLFLAG;
}
