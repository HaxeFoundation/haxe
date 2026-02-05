package cs.system.runtime.interopservices.comtypes;

/** Contains statistical information about an open storage, stream, or byte-array object. */
@:native("System.Runtime.InteropServices.ComTypes.STATSTG")
extern class STATSTG extends cs.system.ValueType {
	/** Specifies the last access time for this storage, stream, or byte array. */
	var atime:cs.system.runtime.interopservices.comtypes.FILETIME;
	/** Specifies the size, in bytes, of the stream or byte array. */
	var cbSize:haxe.Int64;
	/** Indicates the class identifier for the storage object. */
	var clsid:cs.system.Guid;
	/** Indicates the creation time for this storage, stream, or byte array. */
	var ctime:cs.system.runtime.interopservices.comtypes.FILETIME;
	/** Indicates the types of region locking supported by the stream or byte array. */
	var grfLocksSupported:Int;
	/** Indicates the access mode that was specified when the object was opened. */
	var grfMode:Int;
	/** Indicates the current state bits of the storage object (the value most recently set by the  method). */
	var grfStateBits:Int;
	/** Indicates the last modification time for this storage, stream, or byte array. */
	var mtime:cs.system.runtime.interopservices.comtypes.FILETIME;
	/** Represents a pointer to a null-terminated string containing the name of the object described by this structure. */
	var pwcsName:String;
	/** Reserved for future use. */
	var reserved:Int;
	/** Indicates the type of storage object, which is one of the values from the  enumeration. */
	var type:Int;
}
