package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  structure. */
@:native("System.Runtime.InteropServices.ComTypes.STATDATA")
extern class STATDATA extends cs.system.ValueType {
	/** Represents the  enumeration value that determines when the advisory sink is notified of changes in the data. */
	var advf:cs.system.runtime.interopservices.comtypes.ADVF;
	/** Represents the  interface that will receive change notifications. */
	var advSink:cs.system.runtime.interopservices.comtypes.IAdviseSink;
	/** Represents the token that uniquely identifies the advisory connection. This token is returned by the method that sets up the advisory connection. */
	var connection:Int;
	/** Represents the  structure for the data of interest to the advise sink. The advise sink receives notification of changes to the data specified by this  structure. */
	var formatetc:cs.system.runtime.interopservices.comtypes.FORMATETC;
}
