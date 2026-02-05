package cs.system.runtime.interopservices.comtypes;

/** Specifies the direction of the data flow in the  parameter of the  method. This determines the formats that the resulting enumerator can enumerate. */
@:native("System.Runtime.InteropServices.ComTypes.DATADIR")
extern enum DATADIR {
	DATADIR_GET;
	DATADIR_SET;
}
