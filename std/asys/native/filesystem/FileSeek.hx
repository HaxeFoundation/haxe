package asys.native.filesystem;

import haxe.Int64;

/**
	Modes for moving file position pointer
*/
enum abstract FileSeek(String) {
	/** Set the pointer to the exact position specified */
	var SeekSet;
	/** Set the pointer position relative to the end-of-file */
	var SeekEnd;
	/** Set the pointer position relative to the current position */
	var SeekMove;
}