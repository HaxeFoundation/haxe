package asys.native.filesystem;

import asys.native.filesystem.File;
import haxe.io.Bytes;

enum abstract FileOpenFlag<T>(Int) {
	/**
		Open file for appending.
		The file is created if it does not exist.
	**/
	var Append:FileOpenFlag<FileAppend>;

	/**
		Open file for reading.
		Fails if the file does not exist.
	**/
	var Read:FileOpenFlag<FileRead>;

	/**
		Open file for reading and writing.
		Fails if the file does not exist.
	**/
	var ReadWrite:FileOpenFlag<File>;

	/**
		Open file for writing.
		The file is truncated if it exists.
		The file is created if it doesn't exist.
	**/
	var Write:FileOpenFlag<FileWrite>;

	/**
		Create and open file for writing.
		Fails if the file already exists.
	**/
	var WriteX:FileOpenFlag<FileWrite>;

	/**
		Open file for writing and reading.
		The file is truncated if it exists.
		The file is created if it doesn't exist.
	**/
	var WriteRead:FileOpenFlag<File>;

	/**
		Create and open file for writing and reading.
		Fails if the file already exists.
	**/
	var WriteReadX:FileOpenFlag<File>;

	/**
		Open file for writing.
		The file is _not_ truncated if it exists (as opposed to `Write`).
		The file is created if it doesn't exist.
	**/
	var Overwrite:FileOpenFlag<FileWrite>;

	/**
		Open file for writing and reading.
		The file is _not_ truncated if it exists (as opposed to `WriteRead`).
		The file is created if it doesn't exist.
	**/
	var OverwriteRead:FileOpenFlag<File>;
}

/**
	Limits file operations to reading.
	@see asys.native.filesystem.File
**/
@:forward(path,read,info,setPermissions,setOwner,setGroup,setTimes,lock,close,isOpen)
abstract FileRead(File) from File {}

/**
	Limits file operations to writing.
	@see asys.native.filesystem.File
**/
@:forward(path,write,flush,sync,info,setPermissions,setOwner,setGroup,setTimes,lock,resize,close,isOpen)
abstract FileWrite(File) from File {}

/**
	Limits file operations to writing at the end of file and reading.
	@see asys.native.filesystem.File
**/
@:forward(path,read,flush,sync,info,setPermissions,setOwner,setGroup,setTimes,lock,resize,close,isOpen)
abstract FileAppendRead(File) from File {
	/**
		Append up to `length` bytes from `buffer` starting at the buffer `offset`
		to the file, then invoke `callback` with the amount of bytes written.

		If `offset` is outside of `buffer` bounds or if `length` is negative, an
		error passed to the `callback`.

		TODO: Is `append` a better name for this method?
	**/
	public inline function write(buffer:Bytes, offset:Int, length:Int, callback:Callback<Int>):Void {
		this.write(0, buffer, offset, length, callback);
	}
}

/**
	Limits file operations to writing at the end of file.
	@see asys.native.filesystem.File
**/
@:forward(path,write,flush,sync,info,setPermissions,setOwner,setGroup,setTimes,lock,resize,close,isOpen)
abstract FileAppend(FileAppendRead) from File {}
