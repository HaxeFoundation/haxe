package cs.system.runtime.interopservices.comtypes;

/** Contains attributes of a . */
@:native("System.Runtime.InteropServices.ComTypes.TYPEATTR")
extern class TYPEATTR extends cs.system.ValueType {
	/** A constant used with the  and  fields. */
	static var MEMBER_ID_NIL(default, never):Int;
	/** Specifies the byte alignment for an instance of this type. */
	var cbAlignment:cs.Int16;
	/** The size of an instance of this type. */
	var cbSizeInstance:Int;
	/** The size of this type's virtual method table (VTBL). */
	var cbSizeVft:cs.Int16;
	/** Indicates the number of functions on the interface this structure describes. */
	var cFuncs:cs.Int16;
	/** Indicates the number of implemented interfaces on the interface this structure describes. */
	var cImplTypes:cs.Int16;
	/** Indicates the number of variables and data fields on the interface described by this structure. */
	var cVars:cs.Int16;
	/** Reserved for future use. */
	var dwReserved:Int;
	/** The GUID of the type information. */
	var guid:cs.system.Guid;
	/** IDL attributes of the described type. */
	var idldescType:cs.system.runtime.interopservices.comtypes.IDLDESC;
	/** Locale of member names and documentation strings. */
	var lcid:Int;
	/** Reserved for future use. */
	var lpstrSchema:cs.system.IntPtr;
	/** ID of constructor, or  if none. */
	var memidConstructor:Int;
	/** ID of destructor, or  if none. */
	var memidDestructor:Int;
	/** If  == , specifies the type for which this type is an alias. */
	var tdescAlias:cs.system.runtime.interopservices.comtypes.TYPEDESC;
	/** A  value describing the type this information describes. */
	var typekind:cs.system.runtime.interopservices.comtypes.TYPEKIND;
	/** Major version number. */
	var wMajorVerNum:cs.Int16;
	/** Minor version number. */
	var wMinorVerNum:cs.Int16;
	/** A  value describing this information. */
	var wTypeFlags:cs.system.runtime.interopservices.comtypes.TYPEFLAGS;
}
