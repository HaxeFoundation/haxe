package cs.system.runtime.interopservices.comtypes;

/** Defines a function description. */
@:native("System.Runtime.InteropServices.ComTypes.FUNCDESC")
extern class FUNCDESC extends cs.system.ValueType {
	/** Specifies the calling convention of a function. */
	var callconv:cs.system.runtime.interopservices.comtypes.CALLCONV;
	/** Counts the total number of parameters. */
	var cParams:cs.Int16;
	/** Counts the optional parameters. */
	var cParamsOpt:cs.Int16;
	/** Counts the permitted return values. */
	var cScodes:cs.Int16;
	/** Contains the return type of the function. */
	var elemdescFunc:cs.system.runtime.interopservices.comtypes.ELEMDESC;
	/** Specifies whether the function is virtual, static, or dispatch-only. */
	var funckind:cs.system.runtime.interopservices.comtypes.FUNCKIND;
	/** Specifies the type of a property function. */
	var invkind:cs.system.runtime.interopservices.comtypes.INVOKEKIND;
	/** Indicates the size of . */
	var lprgelemdescParam:cs.system.IntPtr;
	/** Stores the count of errors a function can return on a 16-bit system. */
	var lprgscode:cs.system.IntPtr;
	/** Identifies the function member ID. */
	var memid:Int;
	/** Specifies the offset in the VTBL for . */
	var oVft:cs.Int16;
	/** Indicates the  of a function. */
	var wFuncFlags:cs.Int16;
}
