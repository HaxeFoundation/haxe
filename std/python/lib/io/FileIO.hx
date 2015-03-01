
package python.lib.io;

import python.lib.io.RawIOBase;

@:pythonImport("io", "FileIO")
extern class FileIO extends RawIOBase {

	/**
		The mode as given in the constructor.
	**/
	public var mode:String;
	/**
		The file name. This is the file descriptor of the file when no name is given in the constructor.
	**/
	public var name:String;



}