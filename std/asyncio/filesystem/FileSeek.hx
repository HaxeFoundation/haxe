package asyncio.filesystem;

/**
	Modes for moving file position pointer
*/
enum abstract FileSeek(Int) {
	/** Set the pointer to the exact position specified by `offset` */
	var SeekSet;
	/** Move the pointer to the end-of-file */
	var SeekEnd;
	/**
		Move the pointer by `offset` bytes.
		If `offset` is positive the pointer is moved towards the end of file.
		If `offset` is negative the pointer is moved towards the beginning of file.
	*/
	var SeekCurrent;
}