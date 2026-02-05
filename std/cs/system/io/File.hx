package cs.system.io;

/** Provides static methods for the creation, copying, deletion, moving, and opening of a single file, and aids in the creation of  objects. */
@:native("System.IO.File")
extern class File {
	@:overload(function(path:String, contents:cs.system.collections.generic.IEnumerable<String>):Void {})
	/**
	 * Appends lines to a file, and then closes the file. If the specified file does
	 * not exist, this method creates a file, writes the specified lines to the file,
	 * and then closes the file.
	 * @param path The file to append the lines to. The file is created if it doesn't
	 * already exist.
	 * @param contents The lines to append to the file.
	 */
	static function AppendAllLines(path:String, contents:cs.system.collections.generic.IEnumerable<String>, encoding:cs.system.text.Encoding):Void;
	@:overload(function(path:String, contents:cs.system.collections.generic.IEnumerable<String>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously appends lines to a file by using a specified encoding, and then
	 * closes the file. If the specified file does not exist, this method creates a
	 * file, writes the specified lines to the file, and then closes the file.
	 * @param path The file to append the lines to. The file is created if it doesn't
	 * already exist.
	 * @param contents The lines to append to the file.
	 * @param encoding The character encoding to use.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous append operation.
	 */
	static function AppendAllLinesAsync(path:String, contents:cs.system.collections.generic.IEnumerable<String>, encoding:cs.system.text.Encoding, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(path:String, contents:String):Void {})
	/**
	 * Opens a file, appends the specified string to the file, and then closes the
	 * file. If the file does not exist, this method creates a file, writes the
	 * specified string to the file, then closes the file.
	 * @param path The file to append the specified string to.
	 * @param contents The string to append to the file.
	 */
	static function AppendAllText(path:String, contents:String, encoding:cs.system.text.Encoding):Void;
	@:overload(function(path:String, contents:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously opens a file or creates the file if it does not already exist,
	 * appends the specified string to the file using the specified encoding, and then
	 * closes the file.
	 * @param path The file to append the specified string to.
	 * @param contents The string to append to the file.
	 * @param encoding The character encoding to use.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous append operation.
	 */
	static function AppendAllTextAsync(path:String, contents:String, encoding:cs.system.text.Encoding, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	/**
	 * Creates a  that appends UTF-8 encoded text to an existing file, or to a new file
	 * if the specified file does not exist.
	 * @param path The path to the file to append to.
	 * @return A stream writer that appends UTF-8 encoded text to the specified file or
	 * to a new file.
	 */
	static function AppendText(path:String):cs.system.io.StreamWriter;
	@:overload(function(sourceFileName:String, destFileName:String):Void {})
	/**
	 * Copies an existing file to a new file. Overwriting a file of the same name is
	 * not allowed.
	 * @param sourceFileName The file to copy.
	 * @param destFileName The name of the destination file. This cannot be a directory
	 * or an existing file.
	 */
	static function Copy(sourceFileName:String, destFileName:String, overwrite:Bool):Void;
	@:overload(function(path:String):cs.system.io.FileStream {})
	@:overload(function(path:String, bufferSize:Int):cs.system.io.FileStream {})
	/**
	 * Creates or overwrites a file in the specified path.
	 * @param path The path and name of the file to create.
	 * @return A  that provides read/write access to the file specified in .
	 */
	static function Create(path:String, bufferSize:Int, options:cs.system.io.FileOptions):cs.system.io.FileStream;
	/**
	 * Creates or opens a file for writing UTF-8 encoded text. If the file already
	 * exists, its contents are overwritten.
	 * @param path The file to be opened for writing.
	 * @return A  that writes to the specified file using UTF-8 encoding.
	 */
	static function CreateText(path:String):cs.system.io.StreamWriter;
	/**
	 * Decrypts a file that was encrypted by the current account using the  method.
	 * @param path A path that describes a file to decrypt.
	 */
	static function Decrypt(path:String):Void;
	/**
	 * Deletes the specified file.
	 * @param path The name of the file to be deleted. Wildcard characters are not
	 * supported.
	 */
	static function Delete(path:String):Void;
	/**
	 * Encrypts a file so that only the account used to encrypt the file can decrypt
	 * it.
	 * @param path A path that describes a file to encrypt.
	 */
	static function Encrypt(path:String):Void;
	/**
	 * Determines whether the specified file exists.
	 * @param path The file to check.
	 * @return if the caller has the required permissions and  contains the name of an
	 * existing file; otherwise, . This method also returns  if  is , an invalid path,
	 * or a zero-length string. If the caller does not have sufficient permissions to
	 * read the specified file, no exception is thrown and the method returns 
	 * regardless of the existence of .
	 */
	static function Exists(path:String):Bool;
	/**
	 * Gets the  of the file on the path.
	 * @param path The path to the file.
	 * @return The  of the file on the path.
	 */
	static function GetAttributes(path:String):cs.system.io.FileAttributes;
	/**
	 * Returns the creation date and time of the specified file or directory.
	 * @param path The file or directory for which to obtain creation date and time
	 * information.
	 * @return A  structure set to the creation date and time for the specified file or
	 * directory. This value is expressed in local time.
	 */
	static function GetCreationTime(path:String):cs.system.DateTime;
	/**
	 * Returns the creation date and time, in coordinated universal time (UTC), of the
	 * specified file or directory.
	 * @param path The file or directory for which to obtain creation date and time
	 * information.
	 * @return A  structure set to the creation date and time for the specified file or
	 * directory. This value is expressed in UTC time.
	 */
	static function GetCreationTimeUtc(path:String):cs.system.DateTime;
	/**
	 * Returns the date and time the specified file or directory was last accessed.
	 * @param path The file or directory for which to obtain access date and time
	 * information.
	 * @return A  structure set to the date and time that the specified file or
	 * directory was last accessed. This value is expressed in local time.
	 */
	static function GetLastAccessTime(path:String):cs.system.DateTime;
	/**
	 * Returns the date and time, in coordinated universal time (UTC), that the
	 * specified file or directory was last accessed.
	 * @param path The file or directory for which to obtain access date and time
	 * information.
	 * @return A  structure set to the date and time that the specified file or
	 * directory was last accessed. This value is expressed in UTC time.
	 */
	static function GetLastAccessTimeUtc(path:String):cs.system.DateTime;
	/**
	 * Returns the date and time the specified file or directory was last written to.
	 * @param path The file or directory for which to obtain write date and time
	 * information.
	 * @return A  structure set to the date and time that the specified file or
	 * directory was last written to. This value is expressed in local time.
	 */
	static function GetLastWriteTime(path:String):cs.system.DateTime;
	/**
	 * Returns the date and time, in coordinated universal time (UTC), that the
	 * specified file or directory was last written to.
	 * @param path The file or directory for which to obtain write date and time
	 * information.
	 * @return A  structure set to the date and time that the specified file or
	 * directory was last written to. This value is expressed in UTC time.
	 */
	static function GetLastWriteTimeUtc(path:String):cs.system.DateTime;
	/**
	 * Moves a specified file to a new location, providing the option to specify a new
	 * file name.
	 * @param sourceFileName The name of the file to move. Can include a relative or
	 * absolute path.
	 * @param destFileName The new path and name for the file.
	 */
	static function Move(sourceFileName:String, destFileName:String):Void;
	@:overload(function(path:String, mode:cs.system.io.FileMode):cs.system.io.FileStream {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess):cs.system.io.FileStream {})
	/**
	 * Opens a  on the specified path with read/write access with no sharing.
	 * @param path The file to open.
	 * @param mode A  value that specifies whether a file is created if one does not
	 * exist, and determines whether the contents of existing files are retained or
	 * overwritten.
	 * @return A  opened in the specified mode and path, with read/write access and not
	 * shared.
	 */
	static function Open(path:String, mode:cs.system.io.FileMode, access:cs.system.io.FileAccess, share:cs.system.io.FileShare):cs.system.io.FileStream;
	/**
	 * Opens an existing file for reading.
	 * @param path The file to be opened for reading.
	 * @return A read-only  on the specified path.
	 */
	static function OpenRead(path:String):cs.system.io.FileStream;
	/**
	 * Opens an existing UTF-8 encoded text file for reading.
	 * @param path The file to be opened for reading.
	 * @return A  on the specified path.
	 */
	static function OpenText(path:String):cs.system.io.StreamReader;
	/**
	 * Opens an existing file or creates a new file for writing.
	 * @param path The file to be opened for writing.
	 * @return An unshared  object on the specified path with  access.
	 */
	static function OpenWrite(path:String):cs.system.io.FileStream;
	/**
	 * Opens a binary file, reads the contents of the file into a byte array, and then
	 * closes the file.
	 * @param path The file to open for reading.
	 * @return A byte array containing the contents of the file.
	 */
	static function ReadAllBytes(path:String):cs.NativeArray<cs.UInt8>;
	/**
	 * Asynchronously opens a binary file, reads the contents of the file into a byte
	 * array, and then closes the file.
	 * @param path The file to open for reading.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous read operation, which wraps the
	 * byte array containing the contents of the file.
	 */
	static function ReadAllBytesAsync(path:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.NativeArray<cs.UInt8>>;
	@:overload(function(path:String):cs.NativeArray<String> {})
	/**
	 * Opens a text file, reads all lines of the file, and then closes the file.
	 * @param path The file to open for reading.
	 * @return A string array containing all lines of the file.
	 */
	static function ReadAllLines(path:String, encoding:cs.system.text.Encoding):cs.NativeArray<String>;
	@:overload(function(path:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.NativeArray<String>> {})
	/**
	 * Asynchronously opens a text file, reads all lines of the file with the specified
	 * encoding, and then closes the file.
	 * @param path The file to open for reading.
	 * @param encoding The encoding applied to the contents of the file.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous read operation, which wraps the
	 * string array containing all lines of the file.
	 */
	static function ReadAllLinesAsync(path:String, encoding:cs.system.text.Encoding, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<cs.NativeArray<String>>;
	@:overload(function(path:String):String {})
	/**
	 * Opens a text file, reads all the text in the file, and then closes the file.
	 * @param path The file to open for reading.
	 * @return A string containing all the text in the file.
	 */
	static function ReadAllText(path:String, encoding:cs.system.text.Encoding):String;
	@:overload(function(path:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<String> {})
	/**
	 * Asynchronously opens a text file, reads all text in the file with the specified
	 * encoding, and then closes the file.
	 * @param path The file to open for reading.
	 * @param encoding The encoding applied to the contents of the file.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous read operation, which wraps the
	 * string containing all text in the file.
	 */
	static function ReadAllTextAsync(path:String, encoding:cs.system.text.Encoding, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task_1<String>;
	@:overload(function(path:String):cs.system.collections.generic.IEnumerable<String> {})
	/**
	 * Reads the lines of a file.
	 * @param path The file to read.
	 * @return All the lines of the file, or the lines that are the result of a query.
	 */
	static function ReadLines(path:String, encoding:cs.system.text.Encoding):cs.system.collections.generic.IEnumerable<String>;
	@:overload(function(sourceFileName:String, destinationFileName:String, destinationBackupFileName:String):Void {})
	/**
	 * Replaces the contents of a specified file with the contents of another file,
	 * deleting the original file, and creating a backup of the replaced file.
	 * @param sourceFileName The name of a file that replaces the file specified by .
	 * @param destinationFileName The name of the file being replaced.
	 * @param destinationBackupFileName The name of the backup file.
	 */
	static function Replace(sourceFileName:String, destinationFileName:String, destinationBackupFileName:String, ignoreMetadataErrors:Bool):Void;
	/**
	 * Sets the specified  of the file on the specified path.
	 * @param path The path to the file.
	 * @param fileAttributes A bitwise combination of the enumeration values.
	 */
	static function SetAttributes(path:String, fileAttributes:cs.system.io.FileAttributes):Void;
	/**
	 * Sets the date and time the file was created.
	 * @param path The file for which to set the creation date and time information.
	 * @param creationTime A  containing the value to set for the creation date and
	 * time of . This value is expressed in local time.
	 */
	static function SetCreationTime(path:String, creationTime:cs.system.DateTime):Void;
	/**
	 * Sets the date and time, in coordinated universal time (UTC), that the file was
	 * created.
	 * @param path The file for which to set the creation date and time information.
	 * @param creationTimeUtc A  containing the value to set for the creation date and
	 * time of . This value is expressed in UTC time.
	 */
	static function SetCreationTimeUtc(path:String, creationTimeUtc:cs.system.DateTime):Void;
	/**
	 * Sets the date and time the specified file was last accessed.
	 * @param path The file for which to set the access date and time information.
	 * @param lastAccessTime A  containing the value to set for the last access date
	 * and time of . This value is expressed in local time.
	 */
	static function SetLastAccessTime(path:String, lastAccessTime:cs.system.DateTime):Void;
	/**
	 * Sets the date and time, in coordinated universal time (UTC), that the specified
	 * file was last accessed.
	 * @param path The file for which to set the access date and time information.
	 * @param lastAccessTimeUtc A  containing the value to set for the last access date
	 * and time of . This value is expressed in UTC time.
	 */
	static function SetLastAccessTimeUtc(path:String, lastAccessTimeUtc:cs.system.DateTime):Void;
	/**
	 * Sets the date and time that the specified file was last written to.
	 * @param path The file for which to set the date and time information.
	 * @param lastWriteTime A  containing the value to set for the last write date and
	 * time of . This value is expressed in local time.
	 */
	static function SetLastWriteTime(path:String, lastWriteTime:cs.system.DateTime):Void;
	/**
	 * Sets the date and time, in coordinated universal time (UTC), that the specified
	 * file was last written to.
	 * @param path The file for which to set the date and time information.
	 * @param lastWriteTimeUtc A  containing the value to set for the last write date
	 * and time of . This value is expressed in UTC time.
	 */
	static function SetLastWriteTimeUtc(path:String, lastWriteTimeUtc:cs.system.DateTime):Void;
	/**
	 * Creates a new file, writes the specified byte array to the file, and then closes
	 * the file. If the target file already exists, it is overwritten.
	 * @param path The file to write to.
	 * @param bytes The bytes to write to the file.
	 */
	static function WriteAllBytes(path:String, bytes:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Asynchronously creates a new file, writes the specified byte array to the file,
	 * and then closes the file. If the target file already exists, it is overwritten.
	 * @param path The file to write to.
	 * @param bytes The bytes to write to the file.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous write operation.
	 */
	static function WriteAllBytesAsync(path:String, bytes:cs.NativeArray<cs.UInt8>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(path:String, contents:cs.system.collections.generic.IEnumerable<String>):Void {})
	@:overload(function(path:String, contents:cs.NativeArray<String>):Void {})
	@:overload(function(path:String, contents:cs.system.collections.generic.IEnumerable<String>, encoding:cs.system.text.Encoding):Void {})
	/**
	 * Creates a new file, writes a collection of strings to the file, and then closes
	 * the file.
	 * @param path The file to write to.
	 * @param contents The lines to write to the file.
	 */
	static function WriteAllLines(path:String, contents:cs.NativeArray<String>, encoding:cs.system.text.Encoding):Void;
	@:overload(function(path:String, contents:cs.system.collections.generic.IEnumerable<String>, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously creates a new file, write the specified lines to the file by
	 * using the specified encoding, and then closes the file.
	 * @param path The file to write to.
	 * @param contents The lines to write to the file.
	 * @param encoding The character encoding to use.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous write operation.
	 */
	static function WriteAllLinesAsync(path:String, contents:cs.system.collections.generic.IEnumerable<String>, encoding:cs.system.text.Encoding, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
	@:overload(function(path:String, contents:String):Void {})
	/**
	 * Creates a new file, writes the specified string to the file, and then closes the
	 * file. If the target file already exists, it is overwritten.
	 * @param path The file to write to.
	 * @param contents The string to write to the file.
	 */
	static function WriteAllText(path:String, contents:String, encoding:cs.system.text.Encoding):Void;
	@:overload(function(path:String, contents:String, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task {})
	/**
	 * Asynchronously creates a new file, writes the specified string to the file using
	 * the specified encoding, and then closes the file. If the target file already
	 * exists, it is overwritten.
	 * @param path The file to write to.
	 * @param contents The string to write to the file.
	 * @param encoding The encoding to apply to the string.
	 * @param cancellationToken The token to monitor for cancellation requests. The
	 * default value is .
	 * @return A task that represents the asynchronous write operation.
	 */
	static function WriteAllTextAsync(path:String, contents:String, encoding:cs.system.text.Encoding, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.Task;
}
