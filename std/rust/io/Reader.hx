package rust.io;
import rust.*;
/** The raw underlying reader trait. All readers must implement this. */
@:native("io.Reader") extern interface Reader {
	/** Read up to [len] bytes (or EOF) and put them into [bytes] (which must be at least len bytes long). Return number of bytes read. */
	public function read(bytes:Array<Int>, len:Int):Int;
	/** Read a single byte, returning a negative value for EOF or read error. */
	public function read_byte():Int;
	/** Return whether the stream is currently at EOF position. */
	public function eof():Bool;
	/** Move the current position within the stream. The second parameter determines the position that the first parameter is relative to. */
	public function seek(position:Int, style:SeekStyle):Void;
	/** Return the current position within the stream. */
	public function tell():Int;
}