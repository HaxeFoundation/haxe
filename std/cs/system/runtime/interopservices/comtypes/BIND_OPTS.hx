package cs.system.runtime.interopservices.comtypes;

/** Stores the parameters that are used during a moniker binding operation. */
@:native("System.Runtime.InteropServices.ComTypes.BIND_OPTS")
extern class BIND_OPTS extends cs.system.ValueType {
	/** Specifies the size, in bytes, of the  structure. */
	var cbStruct:Int;
	/** Indicates the amount of time (clock time in milliseconds, as returned by the  function) that the caller specified to complete the binding operation. */
	var dwTickCountDeadline:Int;
	/** Controls aspects of moniker binding operations. */
	var grfFlags:Int;
	/** Represents flags that should be used when opening the file that contains the object identified by the moniker. */
	var grfMode:Int;
}
