package cs.system.net;

/** Provides a file system implementation of the  class. */
@:native("System.Net.FileWebResponse")
extern class FileWebResponse extends cs.system.net.WebResponse {
	/** Closes the response stream. */
	function Close():Void;
	/**
	 * Returns the data stream from the file system resource.
	 * @return A  for reading data from the file system resource.
	 */
	function GetResponseStream():cs.system.io.Stream;
}
