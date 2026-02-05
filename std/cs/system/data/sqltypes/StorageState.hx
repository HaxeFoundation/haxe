package cs.system.data.sqltypes;

/** The  enumeration is not intended for use as a stand-alone component, but as an enumeration from which other classes derive standard functionality. */
@:native("System.Data.SqlTypes.StorageState")
extern enum abstract StorageState(Int) {
	var Buffer = 0;
	var Stream = 1;
	var UnmanagedBuffer = 2;
}
