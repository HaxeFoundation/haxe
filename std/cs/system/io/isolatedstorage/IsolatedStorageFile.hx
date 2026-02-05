package cs.system.io.isolatedstorage;

/** Represents an isolated storage area containing files and directories. */
@:native("System.IO.IsolatedStorage.IsolatedStorageFile")
extern class IsolatedStorageFile extends cs.system.io.isolatedstorage.IsolatedStorage {
	/**
	 * Gets a value that indicates whether isolated storage is enabled.
	 * @return in all cases.
	 */
	static var IsEnabled(default, never):Bool;
	/**
	 * Gets the enumerator for the  stores within an isolated storage scope.
	 * @param scope Represents the  for which to return isolated stores.  and  are the
	 * only  combinations supported.
	 * @return Enumerator for the  stores within the specified isolated storage scope.
	 */
	static function GetEnumerator(scope:cs.system.io.isolatedstorage.IsolatedStorageScope):cs.system.collections.IEnumerator;
	/**
	 * Obtains machine-scoped isolated storage corresponding to the calling code's
	 * application identity.
	 * @return An object corresponding to the isolated storage scope based on the
	 * calling code's application identity.
	 */
	static function GetMachineStoreForApplication():cs.system.io.isolatedstorage.IsolatedStorageFile;
	/**
	 * Obtains machine-scoped isolated storage corresponding to the calling code's
	 * assembly identity.
	 * @return An object corresponding to the isolated storage scope based on the
	 * calling code's assembly identity.
	 */
	static function GetMachineStoreForAssembly():cs.system.io.isolatedstorage.IsolatedStorageFile;
	/**
	 * Obtains machine-scoped isolated storage corresponding to the application domain
	 * identity and the assembly identity.
	 * @return An object corresponding to the , based on a combination of the
	 * application domain identity and the assembly identity.
	 */
	static function GetMachineStoreForDomain():cs.system.io.isolatedstorage.IsolatedStorageFile;
	@:overload(function(scope:cs.system.io.isolatedstorage.IsolatedStorageScope, applicationIdentity:Dynamic):cs.system.io.isolatedstorage.IsolatedStorageFile {})
	@:overload(function(scope:cs.system.io.isolatedstorage.IsolatedStorageScope, applicationEvidenceType:cs.system.Type):cs.system.io.isolatedstorage.IsolatedStorageFile {})
	@:overload(function(scope:cs.system.io.isolatedstorage.IsolatedStorageScope, domainIdentity:Dynamic, assemblyIdentity:Dynamic):cs.system.io.isolatedstorage.IsolatedStorageFile {})
	/**
	 * Obtains isolated storage corresponding to the given application identity.
	 * @param scope A bitwise combination of the enumeration values.
	 * @param applicationIdentity An object that contains evidence for the application
	 * identity.
	 * @return An object that represents the parameters.
	 */
	static function GetStore(scope:cs.system.io.isolatedstorage.IsolatedStorageScope, domainEvidenceType:cs.system.Type, assemblyEvidenceType:cs.system.Type):cs.system.io.isolatedstorage.IsolatedStorageFile;
	/**
	 * Obtains user-scoped isolated storage corresponding to the calling code's
	 * application identity.
	 * @return An object corresponding to the isolated storage scope based on the
	 * calling code's assembly identity.
	 */
	static function GetUserStoreForApplication():cs.system.io.isolatedstorage.IsolatedStorageFile;
	/**
	 * Obtains user-scoped isolated storage corresponding to the calling code's
	 * assembly identity.
	 * @return An object corresponding to the isolated storage scope based on the
	 * calling code's assembly identity.
	 */
	static function GetUserStoreForAssembly():cs.system.io.isolatedstorage.IsolatedStorageFile;
	/**
	 * Obtains user-scoped isolated storage corresponding to the application domain
	 * identity and assembly identity.
	 * @return An object corresponding to the , based on a combination of the
	 * application domain identity and the assembly identity.
	 */
	static function GetUserStoreForDomain():cs.system.io.isolatedstorage.IsolatedStorageFile;
	/**
	 * Obtains a user-scoped isolated store for use by applications in a virtual host
	 * domain.
	 * @return The isolated storage file that corresponds to the isolated storage scope
	 * based on the calling code's application identity.
	 */
	static function GetUserStoreForSite():cs.system.io.isolatedstorage.IsolatedStorageFile;
	/** Removes the isolated storage scope and all its contents. */
	static function Remove(scope:cs.system.io.isolatedstorage.IsolatedStorageScope):Void;
	/** Closes a store previously opened with , , or . */
	function Close():Void;
	@:overload(function(sourceFileName:String, destinationFileName:String):Void {})
	/**
	 * Copies an existing file to a new file.
	 * @param sourceFileName The name of the file to copy.
	 * @param destinationFileName The name of the destination file. This cannot be a
	 * directory or an existing file.
	 */
	function CopyFile(sourceFileName:String, destinationFileName:String, overwrite:Bool):Void;
	/**
	 * Creates a directory in the isolated storage scope.
	 * @param dir The relative path of the directory to create within the isolated
	 * storage scope.
	 */
	function CreateDirectory(dir:String):Void;
	/**
	 * Creates a file in the isolated store.
	 * @param path The relative path of the file to create.
	 * @return A new isolated storage file.
	 */
	function CreateFile(path:String):cs.system.io.isolatedstorage.IsolatedStorageFileStream;
	/**
	 * Deletes a directory in the isolated storage scope.
	 * @param dir The relative path of the directory to delete within the isolated
	 * storage scope.
	 */
	function DeleteDirectory(dir:String):Void;
	/**
	 * Deletes a file in the isolated storage scope.
	 * @param file The relative path of the file to delete within the isolated storage
	 * scope.
	 */
	function DeleteFile(file:String):Void;
	/**
	 * Determines whether the specified path refers to an existing directory in the
	 * isolated store.
	 * @param path The path to test.
	 * @return if  refers to an existing directory in the isolated store and is not ;
	 * otherwise, .
	 */
	function DirectoryExists(path:String):Bool;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/**
	 * Determines whether the specified path refers to an existing file in the isolated
	 * store.
	 * @param path The path and file name to test.
	 * @return if  refers to an existing file in the isolated store and is not ;
	 * otherwise, .
	 */
	function FileExists(path:String):Bool;
	/**
	 * Returns the creation date and time of a specified file or directory.
	 * @param path The path to the file or directory for which to obtain creation date
	 * and time information.
	 * @return The creation date and time for the specified file or directory. This
	 * value is expressed in local time.
	 */
	function GetCreationTime(path:String):cs.system.DateTimeOffset;
	@:overload(function():cs.NativeArray<String> {})
	/**
	 * Enumerates the directories at the root of an isolated store.
	 * @return An array of relative paths of directories at the root of the isolated
	 * store. A zero-length array specifies that there are no directories at the root.
	 */
	function GetDirectoryNames(searchPattern:String):cs.NativeArray<String>;
	@:overload(function():cs.NativeArray<String> {})
	/**
	 * Enumerates the file names at the root of an isolated store.
	 * @return An array of relative paths of files at the root of the isolated store. 
	 * A zero-length array specifies that there are no files at the root.
	 */
	function GetFileNames(searchPattern:String):cs.NativeArray<String>;
	/**
	 * Returns the date and time a specified file or directory was last accessed.
	 * @param path The path to the file or directory for which to obtain last access
	 * date and time information.
	 * @return The date and time that the specified file or directory was last
	 * accessed. This value is expressed in local time.
	 */
	function GetLastAccessTime(path:String):cs.system.DateTimeOffset;
	/**
	 * Returns the date and time a specified file or directory was last written to.
	 * @param path The path to the file or directory for which to obtain last write
	 * date and time information.
	 * @return The date and time that the specified file or directory was last written
	 * to. This value is expressed in local time.
	 */
	function GetLastWriteTime(path:String):cs.system.DateTimeOffset;
	/**
	 * Enables an application to explicitly request a larger quota size, in bytes.
	 * @param newQuotaSize The requested size, in bytes.
	 * @return if the new quota is accepted; otherwise, .
	 */
	function IncreaseQuotaTo(newQuotaSize:haxe.Int64):Bool;
	/**
	 * Moves a specified directory and its contents to a new location.
	 * @param sourceDirectoryName The name of the directory to move.
	 * @param destinationDirectoryName The path to the new location for . This cannot
	 * be the path to an existing directory.
	 */
	function MoveDirectory(sourceDirectoryName:String, destinationDirectoryName:String):Void;
	/**
	 * Moves a specified file to a new location, and optionally lets you specify a new
	 * file name.
	 * @param sourceFileName The name of the file to move.
	 * @param destinationFileName The path to the new location for the file. If a file
	 * name is included, the moved file will have that name.
	 */
	function MoveFile(sourceFileName:String, destinationFileName:String):Void;
	@:overload(function(path:String, mode:cs.system.io.FileMode):cs.system.io.isolatedstorage.IsolatedStorageFileStream {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess):cs.system.io.isolatedstorage.IsolatedStorageFileStream {})
	/**
	 * Opens a file in the specified mode.
	 * @param path The relative path of the file within the isolated store.
	 * @param mode One of the enumeration values that specifies how to open the file.
	 * @return A file that is opened in the specified mode, with read/write access, and
	 * is unshared.
	 */
	function OpenFile(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare):cs.system.io.isolatedstorage.IsolatedStorageFileStream;
	/** Removes the isolated storage scope and all its contents. */
	function Remove():Void;
}
