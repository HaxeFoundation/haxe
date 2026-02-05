package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.ITypeInfo2")
extern interface ITypeInfo2 extends cs.system.runtime.interopservices.comtypes.ITypeInfo {
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
	 * @param pUnkOuter An object that acts as the controlling .
	 * @param riid The IID of the interface that the caller uses to communicate with
	 * the resulting object.
	 * @param ppvObj When this method returns, contains a reference to the created
	 * object. This parameter is passed uninitialized.
	 */
	function CreateInstance(pUnkOuter:Dynamic, riid:cs.Ref<cs.system.Guid>, ppvObj:cs.Ref<Dynamic>):Void;
	/**
	 * Gets all custom data items for the library.
	 * @param pCustData A pointer to , which holds all custom data items.
	 */
	function GetAllCustData(pCustData:cs.system.IntPtr):Void;
	/**
	 * Gets all custom data from the specified function.
	 * @param index The index of the function to get the custom data for.
	 * @param pCustData A pointer to , which holds all custom data items.
	 */
	function GetAllFuncCustData(index:Int, pCustData:cs.system.IntPtr):Void;
	/**
	 * Gets all custom data for the specified implementation type.
	 * @param index The index of the implementation type for the custom data.
	 * @param pCustData A pointer to  which holds all custom data items.
	 */
	function GetAllImplTypeCustData(index:Int, pCustData:cs.system.IntPtr):Void;
	/**
	 * Gets all of the custom data for the specified function parameter.
	 * @param indexFunc The index of the function to get the custom data for.
	 * @param indexParam The index of the parameter of this function to get the custom
	 * data for.
	 * @param pCustData A pointer to , which holds all custom data items.
	 */
	function GetAllParamCustData(indexFunc:Int, indexParam:Int, pCustData:cs.system.IntPtr):Void;
	/**
	 * Gets the variable for the custom data.
	 * @param index The index of the variable to get the custom data for.
	 * @param pCustData A pointer to , which holds all custom data items.
	 */
	function GetAllVarCustData(index:Int, pCustData:cs.system.IntPtr):Void;
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
	 * Gets the custom data.
	 * @param guid The GUID used to identify the data.
	 * @param pVarVal When this method returns, contains an  that specifies where to
	 * put the retrieved data. This parameter is passed uninitialized.
	 */
	function GetCustData(guid:cs.Ref<cs.system.Guid>, pVarVal:cs.Ref<Dynamic>):Void;
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
	 * Retrieves the documentation string, the complete Help file name and path, the
	 * localization context to use, and the context ID for the library Help topic in
	 * the Help file.
	 * @param memid The member identifier for the type description.
	 * @param pbstrHelpString When this method returns, contains a  that contains the
	 * name of the specified item. If the caller does not need the item name,  can be .
	 * This parameter is passed uninitialized.
	 * @param pdwHelpStringContext When this method returns, contains the Help
	 * localization context. If the caller does not need the Help context,  can be .
	 * This parameter is passed uninitialized.
	 * @param pbstrHelpStringDll When this method returns, contains a  that contains
	 * the fully qualified name of the file containing the DLL used for the Help file.
	 * If the caller does not need the file name,  can be . This parameter is passed
	 * uninitialized.
	 */
	function GetDocumentation2(memid:Int, pbstrHelpString:cs.Ref<String>, pdwHelpStringContext:cs.Ref<Int>, pbstrHelpStringDll:cs.Ref<String>):Void;
	/**
	 * Gets the custom data from the specified function.
	 * @param index The index of the function to get the custom data for.
	 * @param guid The GUID used to identify the data.
	 * @param pVarVal When this method returns, contains an  that specified where to
	 * put the data. This parameter is passed uninitialized.
	 */
	function GetFuncCustData(index:Int, guid:cs.Ref<cs.system.Guid>, pVarVal:cs.Ref<Dynamic>):Void;
	/**
	 * Retrieves the  structure that contains information about a specified function.
	 * @param index The index of the function description to return.
	 * @param ppFuncDesc When this method returns, contains a reference to a  structure
	 * that describes the specified function. This parameter is passed uninitialized.
	 */
	function GetFuncDesc(index:Int, ppFuncDesc:cs.Ref<cs.system.IntPtr>):Void;
	/**
	 * Binds to a specific member based on a known DISPID, where the member name is not
	 * known (for example, when binding to a default member).
	 * @param memid The member identifier.
	 * @param invKind One of the  values that specifies the kind of member identified
	 * by memid.
	 * @param pFuncIndex When this method returns, contains an index into the function.
	 * This parameter is passed uninitialized.
	 */
	function GetFuncIndexOfMemId(memid:Int, invKind:cs.system.runtime.interopservices.comtypes.INVOKEKIND, pFuncIndex:cs.Ref<Int>):Void;
	/**
	 * Maps between member names and member IDs, and parameter names and parameter IDs.
	 * @param rgszNames An array of names to map.
	 * @param cNames The count of names to map.
	 * @param pMemId When this method returns, contains a reference to an array in
	 * which name mappings are placed. This parameter is passed uninitialized.
	 */
	function GetIDsOfNames(rgszNames:cs.NativeArray<String>, cNames:Int, pMemId:cs.NativeArray<Int>):Void;
	/**
	 * Gets the implementation type of the custom data.
	 * @param index The index of the implementation type for the custom data.
	 * @param guid The GUID used to identify the data.
	 * @param pVarVal When this method returns, contains an  that specifies where to
	 * put the retrieved data. This parameter is passed uninitialized.
	 */
	function GetImplTypeCustData(index:Int, guid:cs.Ref<cs.system.Guid>, pVarVal:cs.Ref<Dynamic>):Void;
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
	 * Gets the specified custom data parameter.
	 * @param indexFunc The index of the function to get the custom data for.
	 * @param indexParam The index of the parameter of this function to get the custom
	 * data for.
	 * @param guid The GUID used to identify the data.
	 * @param pVarVal When this method returns, contains an  that specifies where to
	 * put the retrieved data. This parameter is passed uninitialized.
	 */
	function GetParamCustData(indexFunc:Int, indexParam:Int, guid:cs.Ref<cs.system.Guid>, pVarVal:cs.Ref<Dynamic>):Void;
	/**
	 * Retrieves the referenced type descriptions, if a type description references
	 * other type descriptions.
	 * @param hRef A handle to the referenced type description to return.
	 * @param ppTI When this method returns, contains the referenced type description.
	 * This parameter is passed uninitialized.
	 */
	function GetRefTypeInfo(hRef:Int, ppTI:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeInfo>):Void;
	/**
	 * Retrieves the type description of the implemented interface types, if a type
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
	 * @param ppTComp When this method returns, contains a reference to the  of the
	 * containing type library. This parameter is passed uninitialized.
	 */
	function GetTypeComp(ppTComp:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeComp>):Void;
	/**
	 * Returns the type flags without any allocations. This method returns a  type
	 * flag, which expands the type flags without growing the  (type attribute).
	 * @param pTypeFlags When this method returns, contains a  reference to a . This
	 * parameter is passed uninitialized.
	 */
	function GetTypeFlags(pTypeFlags:cs.Ref<Int>):Void;
	/**
	 * Returns the  enumeration quickly, without doing any allocations.
	 * @param pTypeKind When this method returns, contains a reference to a 
	 * enumeration. This parameter is passed uninitialized.
	 */
	function GetTypeKind(pTypeKind:cs.Ref<cs.system.runtime.interopservices.comtypes.TYPEKIND>):Void;
	/**
	 * Gets the variable for the custom data.
	 * @param index The index of the variable to get the custom data for.
	 * @param guid The GUID used to identify the data.
	 * @param pVarVal When this method returns, contains an  that specifies where to
	 * put the retrieved data. This parameter is passed uninitialized.
	 */
	function GetVarCustData(index:Int, guid:cs.Ref<cs.system.Guid>, pVarVal:cs.Ref<Dynamic>):Void;
	/**
	 * Retrieves a  structure that describes the specified variable.
	 * @param index The index of the variable description to return.
	 * @param ppVarDesc When this method returns, contains a reference to the 
	 * structure that describes the specified variable. This parameter is passed
	 * uninitialized.
	 */
	function GetVarDesc(index:Int, ppVarDesc:cs.Ref<cs.system.IntPtr>):Void;
	/**
	 * Binds to a specific member based on a known , where the member name is not known
	 * (for example, when binding to a default member).
	 * @param memid The member identifier.
	 * @param pVarIndex When this method returns, contains an index of . This parameter
	 * is passed uninitialized.
	 */
	function GetVarIndexOfMemId(memid:Int, pVarIndex:cs.Ref<Int>):Void;
	/**
	 * Invokes a method, or accesses a property of an object, that implements the
	 * interface described by the type description.
	 * @param pvInstance A reference to the interface described by this type
	 * description.
	 * @param memid Identifier of the interface member.
	 * @param wFlags Flags describing the context of the invoke call.
	 * @param pDispParams A reference to a structure that contains an array of
	 * arguments, an array of DISPIDs for named arguments, and counts of the number of
	 * elements in each array.
	 * @param pVarResult A reference to the location at which the result is to be
	 * stored. If  specifies  or ,  is ignored. Set to  if no result is desired.
	 * @param pExcepInfo A pointer to an exception information structure, which is
	 * filled in only if  is returned.
	 * @param puArgErr If  returns ,  indicates the index of the argument with
	 * incorrect type. If more than one argument returns an error,  indicates only the
	 * first argument with an error.
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
