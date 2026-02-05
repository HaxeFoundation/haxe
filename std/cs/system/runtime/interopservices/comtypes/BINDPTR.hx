package cs.system.runtime.interopservices.comtypes;

/** Contains a pointer to a bound-to  structure,  structure, or an  interface. */
@:native("System.Runtime.InteropServices.ComTypes.BINDPTR")
extern class BINDPTR extends cs.system.ValueType {
	/** Represents a pointer to a  structure. */
	var lpfuncdesc:cs.system.IntPtr;
	/** Represents a pointer to an  interface. */
	var lptcomp:cs.system.IntPtr;
	/** Represents a pointer to a  structure. */
	var lpvardesc:cs.system.IntPtr;
}
