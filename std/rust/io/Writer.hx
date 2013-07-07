package rust.io;

@:native("io.Writer") extern interface Writer {
	/** Write all of the given bytes. */
	public function write(bytes:Array<Int>):Void;
	/** Move the current position within the stream to [to]. [rel] determines the position that [to] is relative to. */
	public function seek(to:Int, rel:SeekStyle):Void;
	/** Return the current position within the stream. */
	public function tell():Int;
	/** Flush the output buffer for this stream (if there is one). */
	public function flush():Int;
	/** Determine if this Writer is writing to a file or not. */
	public function get_type():WriterType;
}