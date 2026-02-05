package cs.system.runtime.interopservices.comtypes;

/** Contains the type description and process transfer information for a variable, function, or a function parameter. */
@:native("System.Runtime.InteropServices.ComTypes.ELEMDESC")
extern class ELEMDESC extends cs.system.ValueType {
	/** Contains information about an element. */
	var desc:cs.system.runtime.interopservices.comtypes.ELEMDESC_DESCUNION;
	/** Identifies the type of the element. */
	var tdesc:cs.system.runtime.interopservices.comtypes.TYPEDESC;
}
