package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  structure. */
@:native("System.Runtime.InteropServices.ComTypes.STGMEDIUM")
extern class STGMEDIUM extends cs.system.ValueType {
	/** Represents a pointer to an interface instance that allows the sending process to control the way the storage is released when the receiving process calls the  function. If  is ,  uses default procedures to release the storage; otherwise,  uses the specified  interface. */
	var pUnkForRelease:Dynamic;
	/** Specifies the type of storage medium. The marshaling and unmarshaling routines use this value to determine which union member was used. This value must be one of the elements of the  enumeration. */
	var tymed:cs.system.runtime.interopservices.comtypes.TYMED;
	/** Represents a handle, string, or interface pointer that the receiving process can use to access the data being transferred. */
	var unionmember:cs.system.IntPtr;
}
