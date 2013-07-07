package rust.io;

/** Generic utility functions defined on readers.*/
@:native("std.io.ReaderUtil") extern interface ReaderUtil {
	/** Read len bytes into a new vec. */
	public function read_bytes(len:Int):Array<Int>;
	/** Read up until a specified character (which is optionally included) or EOF. */
	public function read_until(c:Int, include:Bool):String;
	/** Read up until the first '\n' char (which is not returned), or EOF. */
	public function read_line():String;
	/** Read up until the first null byte (which is not returned), or EOF. */
	public function read_c_str():String;
	/** Read all the data remaining in the stream in one go. */
	public function read_whole_stream():Array<Int>;
	/** Iterate over every byte until the iterator breaks or EOF. */
	public function each_byte(it:Int -> Bool):Void;
	/** Iterate over every char until the iterator breaks or EOF. */
	public function each_char(it:String -> Bool):Void;
	/** Read all the lines of the file into a vector. */
	public function read_lines():Array<String>;
}