package asyncio.filesystem;

/**
	File locking modes.
	@see asyncio.filesystem.FileSystem.lock
**/
enum abstract FileLock(Int) {
	/**
		Shared lock.
		Useful for reading a file.
	**/
	var Shared;
	/**
		Exclusive lock.
		Useful for writing to a file.
	**/
	var Exclusive;
	/**
		Release a lock.
	**/
	var Unlock;
}