package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.ITypeLib")
extern interface ITypeLib {
	/**
	 * Finds occurrences of a type description in a type library.
	 * @param szNameBuf The name to search for. This is an in/out parameter.
	 * @param lHashVal A hash value to speed up the search, computed by the  function.
	 * If  is 0, a value is computed.
	 * @param ppTInfo When this method returns, contains an array of pointers to the
	 * type descriptions that contain the name specified in . This parameter is passed
	 * uninitialized.
	 * @param rgMemId An array of the  's of the found items;  [i] is the  that indexes
	 * into the type description specified by  [i]. Cannot be .
	 * @param pcFound On entry, indicates how many instances to look for. For example, 
	 * = 1 can be called to find the first occurrence. The search stops when one
	 * instance is found. On exit, indicates the number of instances that were found.
	 * If the  and  values of  are identical, there might be more type descriptions
	 * that contain the name.
	 */
	function FindName(szNameBuf:String, lHashVal:Int, ppTInfo:cs.NativeArray<cs.system.runtime.interopservices.comtypes.ITypeInfo>, rgMemId:cs.NativeArray<Int>, pcFound:cs.Ref<cs.Int16>):Void;
	/**
	 * Retrieves the library's documentation string, the complete Help file name and
	 * path, and the context identifier for the library Help topic in the Help file.
	 * @param index The index of the type description whose documentation is to be
	 * returned.
	 * @param strName When this method returns, contains a string that represents the
	 * name of the specified item. This parameter is passed uninitialized.
	 * @param strDocString When this method returns, contains a string that represents
	 * the documentation string for the specified item. This parameter is passed
	 * uninitialized.
	 * @param dwHelpContext When this method returns, contains the Help context
	 * identifier associated with the specified item. This parameter is passed
	 * uninitialized.
	 * @param strHelpFile When this method returns, contains a string that represents
	 * the fully qualified name of the Help file. This parameter is passed
	 * uninitialized.
	 */
	function GetDocumentation(index:Int, strName:cs.Ref<String>, strDocString:cs.Ref<String>, dwHelpContext:cs.Ref<Int>, strHelpFile:cs.Ref<String>):Void;
	/**
	 * Retrieves the structure that contains the library's attributes.
	 * @param ppTLibAttr When this method returns, contains a structure that contains
	 * the library's attributes. This parameter is passed uninitialized.
	 */
	function GetLibAttr(ppTLibAttr:cs.Ref<cs.system.IntPtr>):Void;
	/**
	 * Enables a client compiler to bind to a library's types, variables, constants,
	 * and global functions.
	 * @param ppTComp When this method returns, contains an instance of a  instance for
	 * this . This parameter is passed uninitialized.
	 */
	function GetTypeComp(ppTComp:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeComp>):Void;
	/**
	 * Retrieves the specified type description in the library.
	 * @param index The index of the  interface to return.
	 * @param ppTI When this method returns, contains an  describing the type
	 * referenced by . This parameter is passed uninitialized.
	 */
	function GetTypeInfo(index:Int, ppTI:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeInfo>):Void;
	/**
	 * Returns the number of type descriptions in the type library.
	 * @return The number of type descriptions in the type library.
	 */
	function GetTypeInfoCount():Int;
	/**
	 * Retrieves the type description that corresponds to the specified GUID.
	 * @param guid The IID of the interface or CLSID of the class whose type info is
	 * requested.
	 * @param ppTInfo When this method returns, contains the requested  interface. This
	 * parameter is passed uninitialized.
	 */
	function GetTypeInfoOfGuid(guid:cs.Ref<cs.system.Guid>, ppTInfo:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeInfo>):Void;
	/**
	 * Retrieves the type of a type description.
	 * @param index The index of the type description within the type library.
	 * @param pTKind When this method returns, contains a reference to the  enumeration
	 * for the type description. This parameter is passed uninitialized.
	 */
	function GetTypeInfoType(index:Int, pTKind:cs.Ref<cs.system.runtime.interopservices.comtypes.TYPEKIND>):Void;
	/**
	 * Indicates whether a passed-in string contains the name of a type or member
	 * described in the library.
	 * @param szNameBuf The string to test. This is an in/out parameter.
	 * @param lHashVal The hash value of .
	 * @return if  was found in the type library; otherwise, .
	 */
	function IsName(szNameBuf:String, lHashVal:Int):Bool;
	/**
	 * Releases the  structure originally obtained from the  method.
	 * @param pTLibAttr The  structure to release.
	 */
	function ReleaseTLibAttr(pTLibAttr:cs.system.IntPtr):Void;
}
