package cs.system.runtime.interopservices.comtypes;

/** Represents a generalized Clipboard format. */
@:native("System.Runtime.InteropServices.ComTypes.FORMATETC")
extern class FORMATETC extends cs.system.ValueType {
	/** Specifies the particular clipboard format of interest. */
	var cfFormat:cs.Int16;
	/** Specifies one of the  enumeration constants that indicates how much detail should be contained in the rendering. */
	var dwAspect:cs.system.runtime.interopservices.comtypes.DVASPECT;
	/** Specifies part of the aspect when the data must be split across page boundaries. */
	var lindex:Int;
	/** Specifies a pointer to a  structure containing information about the target device that the data is being composed for. */
	var ptd:cs.system.IntPtr;
	/** Specifies one of the  enumeration constants, which indicates the type of storage medium used to transfer the object's data. */
	var tymed:cs.system.runtime.interopservices.comtypes.TYMED;
}
