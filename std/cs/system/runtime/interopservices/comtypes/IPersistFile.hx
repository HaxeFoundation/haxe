package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface, with functionality from . */
@:native("System.Runtime.InteropServices.ComTypes.IPersistFile")
extern interface IPersistFile {
	/**
	 * Retrieves the class identifier (CLSID) of an object.
	 * @param pClassID When this method returns, contains a reference to the CLSID.
	 * This parameter is passed uninitialized.
	 */
	function GetClassID(pClassID:cs.Ref<cs.system.Guid>):Void;
	/**
	 * Retrieves either the absolute path to the current working file of the object or,
	 * if there is no current working file, the default file name prompt of the object.
	 * @param ppszFileName When this method returns, contains the address of a pointer
	 * to a zero-terminated string containing the path for the current file, or the
	 * default file name prompt (such as *.txt). This parameter is passed
	 * uninitialized.
	 */
	function GetCurFile(ppszFileName:cs.Ref<String>):Void;
	/**
	 * Checks an object for changes since it was last saved to its current file.
	 * @return if the file has changed since it was last saved;  if the file has not
	 * changed since it was last saved.
	 */
	function IsDirty():Int;
	/**
	 * Opens the specified file and initializes an object from the file contents.
	 * @param pszFileName A zero-terminated string containing the absolute path of the
	 * file to open.
	 * @param dwMode A combination of values from the  enumeration to indicate the
	 * access mode in which to open .
	 */
	function Load(pszFileName:String, dwMode:Int):Void;
	/**
	 * Saves a copy of the object into the specified file.
	 * @param pszFileName A zero-terminated string containing the absolute path of the
	 * file to which the object is saved.
	 * @param fRemember to used the  parameter as the current working file; otherwise .
	 */
	function Save(pszFileName:String, fRemember:Bool):Void;
	/**
	 * Notifies the object that it can write to its file.
	 * @param pszFileName The absolute path of the file where the object was previously
	 * saved.
	 */
	function SaveCompleted(pszFileName:String):Void;
}
