package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the Component Automation ITypeInfo interface. */
@:native("System.Runtime.InteropServices.ComTypes.ITypeInfo")
extern interface ITypeInfo {
	/**
	 * Retrieves the addresses of static functions or variables, such as those defined
	 * in a DLL.
	 * @param memid The member ID of the  member's address to retrieve.
	 * @param invKind One of the  values that specifies whether the member is a
	 * property, and if so, what kind.
	 * @param ppv When this method returns, contains a reference to the  member. This
	 * parameter is passed uninitialized.
	 */
	function AddressOfMember(memid:Int, invKind:cs.system.runtime.interopservices.comtypes.INVOKEKIND, ppv:cs.Ref<cs.system.IntPtr>):Void;
	/**
	 * Creates a new instance of a type that describes a component class (coclass).
	 * @param pUnkOuter The object that acts as the controlling .
	 * @param riid The IID of the interface that the caller uses to communicate with
	 * the resulting object.
	 * @param ppvObj When this method returns, contains a reference to the created
	 * object. This parameter is passed uninitialized.
	 */
	function CreateInstance(pUnkOuter:Dynamic, riid:cs.Ref<cs.system.Guid>, ppvObj:cs.Ref<Dynamic>):Void;
	/**
	 * Retrieves the type library that contains this type description and its index
	 * within that type library.
	 * @param ppTLB When this method returns, contains a reference to the containing
	 * type library. This parameter is passed uninitialized.
	 * @param pIndex When this method returns, contains a reference to the index of the
	 * type description within the containing type library. This parameter is passed
	 * uninitialized.
	 */
	function GetContainingTypeLib(ppTLB:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeLib>, pIndex:cs.Ref<Int>):Void;
	/**
	 * Retrieves a description or specification of an entry point for a function in a
	 * DLL.
	 * @param memid The ID of the member function whose DLL entry description is to be
	 * returned.
	 * @param invKind One of the  values that specifies the kind of member identified
	 * by .
	 * @param pBstrDllName If not , the function sets  to a  that contains the name of
	 * the DLL.
	 * @param pBstrName If not , the function sets lpbstrName to a  that contains the
	 * name of the entry point.
	 * @param pwOrdinal If not , and the function is defined by an ordinal, then
	 * lpwOrdinal is set to point to the ordinal.
	 */
	function GetDllEntry(memid:Int, invKind:cs.system.runtime.interopservices.comtypes.INVOKEKIND, pBstrDllName:cs.system.IntPtr, pBstrName:cs.system.IntPtr, pwOrdinal:cs.system.IntPtr):Void;
	/**
	 * Retrieves the documentation string, the complete Help file name and path, and
	 * the context ID for the Help topic for a specified type description.
	 * @param index The ID of the member whose documentation is to be returned.
	 * @param strName When this method returns, contains the name of the item method.
	 * This parameter is passed uninitialized.
	 * @param strDocString When this method returns, contains the documentation string
	 * for the specified item. This parameter is passed uninitialized.
	 * @param dwHelpContext When this method returns, contains a reference to the Help
	 * context associated with the specified item. This parameter is passed
	 * uninitialized.
	 * @param strHelpFile When this method returns, contains the fully qualified name
	 * of the Help file. This parameter is passed uninitialized.
	 */
	function GetDocumentation(index:Int, strName:cs.Ref<String>, strDocString:cs.Ref<String>, dwHelpContext:cs.Ref<Int>, strHelpFile:cs.Ref<String>):Void;
	/**
	 * Retrieves the  structure that contains information about a specified function.
	 * @param index The index of the function description to return.
	 * @param ppFuncDesc When this method returns, contains a reference to a  structure
	 * that describes the specified function. This parameter is passed uninitialized.
	 */
	function GetFuncDesc(index:Int, ppFuncDesc:cs.Ref<cs.system.IntPtr>):Void;
	/**
	 * Maps between member names and member IDs, and parameter names and parameter IDs.
	 * @param rgszNames An array of names to map.
	 * @param cNames The count of names to map.
	 * @param pMemId When this method returns, contains a reference to an array in
	 * which name mappings are placed. This parameter is passed uninitialized.
	 */
	function GetIDsOfNames(rgszNames:cs.NativeArray<String>, cNames:Int, pMemId:cs.NativeArray<Int>):Void;
	/**
	 * Retrieves the  value for one implemented interface or base interface in a type
	 * description.
	 * @param index The index of the implemented interface or base interface.
	 * @param pImplTypeFlags When this method returns, contains a reference to the 
	 * enumeration. This parameter is passed uninitialized.
	 */
	function GetImplTypeFlags(index:Int, pImplTypeFlags:cs.Ref<cs.system.runtime.interopservices.comtypes.IMPLTYPEFLAGS>):Void;
	/**
	 * Retrieves marshaling information.
	 * @param memid The member ID that indicates which marshaling information is
	 * needed.
	 * @param pBstrMops When this method returns, contains a reference to the  string
	 * used in marshaling the fields of the structure described by the referenced type
	 * description, or returns  if there is no information to return. This parameter is
	 * passed uninitialized.
	 */
	function GetMops(memid:Int, pBstrMops:cs.Ref<String>):Void;
	/**
	 * Retrieves the variable with the specified member ID (or the name of the property
	 * or method and its parameters) that corresponds to the specified function ID.
	 * @param memid The ID of the member whose name (or names) is to be returned.
	 * @param rgBstrNames When this method returns, contains the name (or names)
	 * associated with the member. This parameter is passed uninitialized.
	 * @param cMaxNames The length of the  array.
	 * @param pcNames When this method returns, contains the number of names in the 
	 * array. This parameter is passed uninitialized.
	 */
	function GetNames(memid:Int, rgBstrNames:cs.NativeArray<String>, cMaxNames:Int, pcNames:cs.Ref<Int>):Void;
	/**
	 * Retrieves the referenced type descriptions if a type description references
	 * other type descriptions.
	 * @param hRef A handle to the referenced type description to return.
	 * @param ppTI When this method returns, contains the referenced type description.
	 * This parameter is passed uninitialized.
	 */
	function GetRefTypeInfo(hRef:Int, ppTI:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeInfo>):Void;
	/**
	 * Retrieves the type description of the implemented interface types if a type
	 * description describes a COM class.
	 * @param index The index of the implemented type whose handle is returned.
	 * @param href When this method returns, contains a reference to a handle for the
	 * implemented interface. This parameter is passed uninitialized.
	 */
	function GetRefTypeOfImplType(index:Int, href:cs.Ref<Int>):Void;
	/**
	 * Retrieves a  structure that contains the attributes of the type description.
	 * @param ppTypeAttr When this method returns, contains a reference to the
	 * structure that contains the attributes of this type description. This parameter
	 * is passed uninitialized.
	 */
	function GetTypeAttr(ppTypeAttr:cs.Ref<cs.system.IntPtr>):Void;
	/**
	 * Retrieves the  interface for the type description, which enables a client
	 * compiler to bind to the type description's members.
	 * @param ppTComp When this method returns, contains a reference to the  interface
	 * of the containing type library. This parameter is passed uninitialized.
	 */
	function GetTypeComp(ppTComp:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeComp>):Void;
	/**
	 * Retrieves a  structure that describes the specified variable.
	 * @param index The index of the variable description to return.
	 * @param ppVarDesc When this method returns, contains a reference to the 
	 * structure that describes the specified variable. This parameter is passed
	 * uninitialized.
	 */
	function GetVarDesc(index:Int, ppVarDesc:cs.Ref<cs.system.IntPtr>):Void;
	/**
	 * Invokes a method, or accesses a property of an object, that implements the
	 * interface described by the type description.
	 * @param pvInstance A reference to the interface described by this type
	 * description.
	 * @param memid A value that identifies the interface member.
	 * @param wFlags Flags that describe the context of the invoke call.
	 * @param pDispParams A reference to a structure that contains an array of
	 * arguments, an array of DISPIDs for named arguments, and counts of the number of
	 * elements in each array.
	 * @param pVarResult A reference to the location at which the result is to be
	 * stored. If  specifies  or ,  is ignored. Set to  if no result is desired.
	 * @param pExcepInfo A pointer to an exception information structure, which is
	 * filled in only if  is returned.
	 * @param puArgErr If  returns ,  indicates the index within rgvarg of the argument
	 * with the incorrect type. If more than one argument returns an error,  indicates
	 * only the first argument with an error. This parameter is passed uninitialized.
	 */
	function Invoke(pvInstance:Dynamic, memid:Int, wFlags:cs.Int16, pDispParams:cs.Ref<cs.system.runtime.interopservices.comtypes.DISPPARAMS>, pVarResult:cs.system.IntPtr, pExcepInfo:cs.system.IntPtr, puArgErr:cs.Ref<Int>):Void;
	/**
	 * Releases a  structure previously returned by the  method.
	 * @param pFuncDesc A reference to the  structure to release.
	 */
	function ReleaseFuncDesc(pFuncDesc:cs.system.IntPtr):Void;
	/**
	 * Releases a  structure previously returned by the  method.
	 * @param pTypeAttr A reference to the  structure to release.
	 */
	function ReleaseTypeAttr(pTypeAttr:cs.system.IntPtr):Void;
	/**
	 * Releases a  structure previously returned by the  method.
	 * @param pVarDesc A reference to the  structure to release.
	 */
	function ReleaseVarDesc(pVarDesc:cs.system.IntPtr):Void;
}
