package asys.native.filesystem;

import haxe.Int64;

/**
	Modes for moving file position pointer
*/
enum abstract FileSeek(Int) {
	/** Set the pointer to the exact position specified */
	var SeekSet = 0;
	/** Set the pointer position relative to the current position */
	var SeekMove = 1;
	/** Set the pointer position relative to the end-of-file */
	var SeekEnd = 2;
}