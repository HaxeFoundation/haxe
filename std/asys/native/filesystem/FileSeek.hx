package asys.native.filesystem;

import haxe.Int64;

/**
	Modes for moving file position pointer
*/
enum FileSeek {
	/** Set the pointer to the exact position specified by `offset` */
	SeekSet(offset:Int64);
	/** Move the pointer to the end-of-file */
	SeekEnd;
	/**
		Move the pointer by `offset` bytes.
		If `offset` is positive the pointer is moved towards the end of file.
		If `offset` is negative the pointer is moved towards the beginning of file.
	*/
	SeekMove(offset:Int);
}