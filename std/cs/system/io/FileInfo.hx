package cs.system.io;

/** Provides properties and instance methods for the creation, copying, deletion, moving, and opening of files, and aids in the creation of  objects. This class cannot be inherited. */
@:native("System.IO.FileInfo")
extern class FileInfo extends cs.system.io.FileSystemInfo {
	/**
	 * Gets an instance of the parent directory.
	 * @return A  object representing the parent directory of this file.
	 */
	var Directory(default, never):cs.system.io.DirectoryInfo;
	/**
	 * Gets a string representing the directory's full path.
	 * @return A string representing the directory's full path.
	 */
	var DirectoryName(default, never):String;
	/**
	 * Gets or sets a value that determines if the current file is read only.
	 * @return if the current file is read only; otherwise, .
	 */
	var IsReadOnly(default, default):Bool;
	/**
	 * Gets the size, in bytes, of the current file.
	 * @return The size of the current file in bytes.
	 */
	var Length(default, never):haxe.Int64;
	function new(fileName:String):Void;
	/**
	 * Creates a  that appends text to the file represented by this instance of the .
	 * @return A new .
	 */
	function AppendText():cs.system.io.StreamWriter;
	@:overload(function(destFileName:String):cs.system.io.FileInfo {})
	/**
	 * Copies an existing file to a new file, disallowing the overwriting of an
	 * existing file.
	 * @param destFileName The name of the new file to copy to.
	 * @return A new file with a fully qualified path.
	 */
	function CopyTo(destFileName:String, overwrite:Bool):cs.system.io.FileInfo;
	/**
	 * Creates a file.
	 * @return A new file.
	 */
	function Create():cs.system.io.FileStream;
	/**
	 * Creates a  that writes a new text file.
	 * @return A new .
	 */
	function CreateText():cs.system.io.StreamWriter;
	/** Decrypts a file that was encrypted by the current account using the  method. */
	function Decrypt():Void;
	/** Permanently deletes a file. */
	function Delete():Void;
	/** Encrypts a file so that only the account used to encrypt the file can decrypt it. */
	function Encrypt():Void;
	/**
	 * Moves a specified file to a new location, providing the option to specify a new
	 * file name.
	 * @param destFileName The path to move the file to, which can specify a different
	 * file name.
	 */
	function MoveTo(destFileName:String):Void;
	@:overload(function(mode:cs.system.io.FileMode):cs.system.io.FileStream {})
	@:overload(function(mode:cs.system.io.FileMode, access:cs.system.io.FileAccess):cs.system.io.FileStream {})
	/**
	 * Opens a file in the specified mode.
	 * @param mode A  constant specifying the mode (for example,  or ) in which to open
	 * the file.
	 * @return A file opened in the specified mode, with read/write access and
	 * unshared.
	 */
	function Open(mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare):cs.system.io.FileStream;
	/**
	 * Creates a read-only .
	 * @return A new read-only  object.
	 */
	function OpenRead():cs.system.io.FileStream;
	/**
	 * Creates a  with UTF8 encoding that reads from an existing text file.
	 * @return A new  with UTF8 encoding.
	 */
	function OpenText():cs.system.io.StreamReader;
	/**
	 * Creates a write-only .
	 * @return A write-only unshared  object for a new or existing file.
	 */
	function OpenWrite():cs.system.io.FileStream;
	@:overload(function(destinationFileName:String, destinationBackupFileName:String):cs.system.io.FileInfo {})
	/**
	 * Replaces the contents of a specified file with the file described by the current
	 * object, deleting the original file, and creating a backup of the replaced file.
	 * @param destinationFileName The name of a file to replace with the current file.
	 * @param destinationBackupFileName The name of a file with which to create a
	 * backup of the file described by the  parameter.
	 * @return A  object that encapsulates information about the file described by the 
	 * parameter.
	 */
	function Replace(destinationFileName:String, destinationBackupFileName:String, ignoreMetadataErrors:Bool):cs.system.io.FileInfo;
	/**
	 * Returns the path as a string.
	 * @return A string representing the path.
	 */
	function ToString():String;
}
