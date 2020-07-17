package asys.native.filesystem;

import asys.native.filesystem.File;

enum abstract FileOpenFlag<T>(Int) {
	/**
		Open file for appending.
		The file is created if it does not exist.
		The file pointer is placed at the end of the file.
	**/
	var Append:FileOpenFlag<FileAppend>;

	/**
		Open file for appending and reading.
		The file is created if it does not exist.
		The file pointer for reading is placed at the beginning of the file, but
		writing always appends to the end of the file.
	**/
	var AppendRead:FileOpenFlag<File>;

	/**
		Open file for reading.
		Fails if the file does not exist.
		The file pointer is placed at the beginning of the file.
	**/
	var Read:FileOpenFlag<FileRead>;

	/**
		Open file for reading and writing.
		Fails if the file does not exist.
		The file pointer is placed at the beginning of the file.
	**/
	var ReadWrite:FileOpenFlag<File>;

	/**
		Open file for writing.
		The file is truncated if it exists.
		The file is created if it doesn't exist.
	**/
	var Write:FileOpenFlag<FileWrite>;

	/**
		The same as `Write`, but fails if the path exists.
	**/
	var WriteX:FileOpenFlag<FileWrite>;

	/**
		Open file for writing and reading.
		The file is truncated if it exists.
		The file is created if it doesn't exist.
	**/
	var WriteRead:FileOpenFlag<File>;

	/**
		Like `WriteRead`, but fails if the path exists.
	**/
	var WriteReadX:FileOpenFlag<File>;

	/**
		Open file for writing.
		The file is _not_ truncated if it exists (as opposed to `Write`).
		The file is created if it doesn't exist.
		The file pointer is placed at the beginning of the file.
	**/
	var Overwrite:FileOpenFlag<FileWrite>;

	/**
		Open file for writing and reading.
		The file is _not_ truncated if it exists (as opposed to `WriteRead`).
		The file is created if it doesn't exist.
		The file pointer is placed at the beginning of the file.
	**/
	var OverwriteRead:FileOpenFlag<File>;
}