package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.IRunningObjectTable")
extern interface IRunningObjectTable {
	/**
	 * Enumerates the objects currently registered as running.
	 * @param ppenumMoniker When this method returns, contains the new enumerator for
	 * the Running Object Table (ROT). This parameter is passed uninitialized.
	 */
	function EnumRunning(ppenumMoniker:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumMoniker>):Void;
	/**
	 * Returns the registered object if the supplied object name is registered as
	 * running.
	 * @param pmkObjectName A reference to the moniker to search for in the Running
	 * Object Table (ROT).
	 * @param ppunkObject When this method returns, contains the requested running
	 * object. This parameter is passed uninitialized.
	 * @return An  value that indicates the success or failure of the operation.
	 */
	function GetObject(pmkObjectName:cs.system.runtime.interopservices.comtypes.IMoniker, ppunkObject:cs.Ref<Dynamic>):Int;
	/**
	 * Searches for this moniker in the Running Object Table (ROT) and reports the
	 * recorded time of change, if present.
	 * @param pmkObjectName A reference to the moniker to search for in the Running
	 * Object Table (ROT).
	 * @param pfiletime When this object returns, contains the objects last change
	 * time. This parameter is passed uninitialized.
	 * @return An  value that indicates the success or failure of the operation.
	 */
	function GetTimeOfLastChange(pmkObjectName:cs.system.runtime.interopservices.comtypes.IMoniker, pfiletime:cs.Ref<cs.system.runtime.interopservices.comtypes.FILETIME>):Int;
	/**
	 * Determines whether the specified moniker is currently registered in the Running
	 * Object Table (ROT).
	 * @param pmkObjectName A reference to the moniker to search for in the Running
	 * Object Table (ROT).
	 * @return An  value that indicates the success or failure of the operation.
	 */
	function IsRunning(pmkObjectName:cs.system.runtime.interopservices.comtypes.IMoniker):Int;
	/**
	 * Notes the time that a particular object changed so  can report an appropriate
	 * change time.
	 * @param dwRegister The Running Object Table (ROT) entry of the changed object.
	 * @param pfiletime A reference to the object's last change time.
	 */
	function NoteChangeTime(dwRegister:Int, pfiletime:cs.Ref<cs.system.runtime.interopservices.comtypes.FILETIME>):Void;
	/**
	 * Registers that the supplied object has entered the running state.
	 * @param grfFlags Specifies whether the Running Object Table's (ROT) reference to 
	 * is weak or strong, and controls access to the object through its entry in the
	 * ROT.
	 * @param punkObject A reference to the object being registered as running.
	 * @param pmkObjectName A reference to the moniker that identifies .
	 * @return A value that can be used to identify this ROT entry in subsequent calls
	 * to  or .
	 */
	function Register(grfFlags:Int, punkObject:Dynamic, pmkObjectName:cs.system.runtime.interopservices.comtypes.IMoniker):Int;
	/**
	 * Unregisters the specified object from the Running Object Table (ROT).
	 * @param dwRegister The Running Object Table (ROT) entry to revoke.
	 */
	function Revoke(dwRegister:Int):Void;
}
