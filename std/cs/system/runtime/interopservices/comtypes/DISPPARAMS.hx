package cs.system.runtime.interopservices.comtypes;

/** Contains the arguments passed to a method or property by . */
@:native("System.Runtime.InteropServices.ComTypes.DISPPARAMS")
extern class DISPPARAMS extends cs.system.ValueType {
	/** Represents the count of arguments. */
	var cArgs:Int;
	/** Represents the count of named arguments */
	var cNamedArgs:Int;
	/** Represents the dispatch IDs of named arguments. */
	var rgdispidNamedArgs:cs.system.IntPtr;
	/** Represents a reference to the array of arguments. */
	var rgvarg:cs.system.IntPtr;
}
