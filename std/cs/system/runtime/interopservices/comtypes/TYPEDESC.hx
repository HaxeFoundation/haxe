package cs.system.runtime.interopservices.comtypes;

/** Describes the type of a variable, return type of a function, or the type of a function parameter. */
@:native("System.Runtime.InteropServices.ComTypes.TYPEDESC")
extern class TYPEDESC extends cs.system.ValueType {
	/** If the variable is  or , the  field contains a pointer to a  that specifies the element type. */
	var lpValue:cs.system.IntPtr;
	/** Indicates the variant type for the item described by this . */
	var vt:cs.Int16;
}
