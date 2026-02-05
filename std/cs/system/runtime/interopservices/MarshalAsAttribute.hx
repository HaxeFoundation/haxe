package cs.system.runtime.interopservices;

/** Indicates how to marshal the data between managed and unmanaged code. */
@:native("System.Runtime.InteropServices.MarshalAsAttribute")
extern class MarshalAsAttribute extends cs.system.Attribute {
	/** Specifies the element type of the unmanaged  or . */
	var ArraySubType:cs.system.runtime.interopservices.UnmanagedType;
	/** Specifies the parameter index of the unmanaged  attribute used by COM. */
	var IidParameterIndex:Int;
	/** Provides additional information to a custom marshaler. */
	var MarshalCookie:String;
	/** Specifies the fully qualified name of a custom marshaler. */
	var MarshalType:String;
	/** Implements  as a type. */
	var MarshalTypeRef:cs.system.Type;
	/** Indicates the element type of the . */
	var SafeArraySubType:cs.system.runtime.interopservices.VarEnum;
	/** Indicates the user-defined element type of the . */
	var SafeArrayUserDefinedSubType:cs.system.Type;
	/** Indicates the number of elements in the fixed-length array or the number of characters (not bytes) in a string to import. */
	var SizeConst:Int;
	/** Indicates the zero-based parameter that contains the count of array elements, similar to  in COM. */
	var SizeParamIndex:cs.Int16;
	/**
	 * Gets the  value the data is to be marshaled as.
	 * @return The  value the data is to be marshaled as.
	 */
	var Value(default, never):cs.system.runtime.interopservices.UnmanagedType;
	@:overload(function(unmanagedType:cs.Int16):Void {})
	function new(unmanagedType:cs.system.runtime.interopservices.UnmanagedType):Void;
}
