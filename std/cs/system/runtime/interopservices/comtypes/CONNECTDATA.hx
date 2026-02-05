package cs.system.runtime.interopservices.comtypes;

/** Describes a connection that exists to a given connection point. */
@:native("System.Runtime.InteropServices.ComTypes.CONNECTDATA")
extern class CONNECTDATA extends cs.system.ValueType {
	/** Represents a connection token that is returned from a call to . */
	var dwCookie:Int;
	/** Represents a pointer to the  interface on a connected advisory sink. The caller must call  on this pointer when the  structure is no longer needed. */
	var pUnk:Dynamic;
}
