package cs.system.runtime.interopservices.comtypes;

/** Describes a variable, constant, or data member. */
@:native("System.Runtime.InteropServices.ComTypes.VARDESC")
extern class VARDESC extends cs.system.ValueType {
	/** Contains information about a variable. */
	var desc:cs.system.runtime.interopservices.comtypes.VARDESC_DESCUNION;
	/** Contains the variable type. */
	var elemdescVar:cs.system.runtime.interopservices.comtypes.ELEMDESC;
	/** This field is reserved for future use. */
	var lpstrSchema:String;
	/** Indicates the member ID of a variable. */
	var memid:Int;
	/** Defines how to marshal a variable. */
	var varkind:cs.system.runtime.interopservices.comtypes.VARKIND;
	/** Defines the properties of a variable. */
	var wVarFlags:cs.Int16;
}
