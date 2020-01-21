package asyncio.filesystem;

/**
	Modes for moving file position indicator
*/
enum abstract FileSeek(Int) {
	/** Set the indicator to the exact position specified by `offset` */
	var SeekSet;
	/** Move the indicator to the end-of-file */
	var SeekEnd;
	/**
		Move the indicator by `offset` bytes.
		If `offset` is positive the indicator is moved towards the end of file.
		If `offset` is negative the indicator is moved towards the beginning of file.
	*/
	var SeekCurrent;
}