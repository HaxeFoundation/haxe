package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IBindCtx")
extern interface IBindCtx {
	/**
	 * Enumerates the strings that are the keys of the internally maintained table of
	 * contextual object parameters.
	 * @param ppenum When this method returns, contains a reference to the object
	 * parameter enumerator. This parameter is passed uninitialized.
	 */
	function EnumObjectParam(ppenum:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumString>):Void;
	/**
	 * Returns the current binding options stored in the current bind context.
	 * @param pbindopts A pointer to the structure to receive the binding options.
	 */
	function GetBindOptions(pbindopts:cs.Ref<cs.system.runtime.interopservices.comtypes.BIND_OPTS>):Void;
	/**
	 * Looks up the given key in the internally maintained table of contextual object
	 * parameters and returns the corresponding object, if one exists.
	 * @param pszKey The name of the object to search for.
	 * @param ppunk When this method returns, contains the object interface pointer.
	 * This parameter is passed uninitialized.
	 */
	function GetObjectParam(pszKey:String, ppunk:cs.Ref<Dynamic>):Void;
	/**
	 * Returns access to the Running Object Table (ROT) relevant to this binding
	 * process.
	 * @param pprot When this method returns, contains a reference to the Running
	 * Object Table (ROT). This parameter is passed uninitialized.
	 */
	function GetRunningObjectTable(pprot:cs.Ref<cs.system.runtime.interopservices.comtypes.IRunningObjectTable>):Void;
	/**
	 * Registers the passed object as one of the objects that has been bound during a
	 * moniker operation and that should be released when the operation is complete.
	 * @param punk The object to register for release.
	 */
	function RegisterObjectBound(punk:Dynamic):Void;
	/**
	 * Registers the specified object pointer under the specified name in the
	 * internally maintained table of object pointers.
	 * @param pszKey The name to register  with.
	 * @param punk The object to register.
	 */
	function RegisterObjectParam(pszKey:String, punk:Dynamic):Void;
	/** Releases all the objects currently registered with the bind context by using the  method. */
	function ReleaseBoundObjects():Void;
	/**
	 * Removes the object from the set of registered objects that need to be released.
	 * @param punk The object to unregister for release.
	 */
	function RevokeObjectBound(punk:Dynamic):Void;
	/**
	 * Revokes the registration of the object currently found under the specified key
	 * in the internally maintained table of contextual object parameters, if that key
	 * is currently registered.
	 * @param pszKey The key to unregister.
	 * @return An  value if the specified key was successfully removed from the table;
	 * otherwise, an  value.
	 */
	function RevokeObjectParam(pszKey:String):Int;
	/**
	 * Stores a block of parameters in the bind context. These parameters will apply to
	 * later  operations that use this bind context.
	 * @param pbindopts The structure containing the binding options to set.
	 */
	function SetBindOptions(pbindopts:cs.Ref<cs.system.runtime.interopservices.comtypes.BIND_OPTS>):Void;
}
