package cs.system.data.sqltypes;

/** The  enumeration is not intended for use as a stand-alone component, but as an enumeration from which other classes derive standard functionality. */
@:native("System.Data.SqlTypes.StorageState")
extern enum StorageState {
	Buffer;
	Stream;
	UnmanagedBuffer;
}
