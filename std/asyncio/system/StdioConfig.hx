package asyncio.system;

import asyncio.filesystem.File;
import asyncio.filesystem.FileOpenFlag;
import asyncio.filesystem.FilePath;

/**
	This enum allows to configure IO channels of a process being created with functions
	like `asyncio.system.Process.open` and such.
	@see asyncio.system.Process
**/
enum StdioConfig {
	/**
		Create a unidirectional pipe for IO channel.
		The child process will be able to read from the pipe, while the parent
		process will be able to write into the pipe.
		This is the default behavior for stdin.
	**/
	PipeRead;
	/**
		Create a unidirectional pipe for IO channel.
		The child process will be able to read from the pipe, while the parent
		process will be able to write into the pipe.
		This is the default behavior for stdout and stderr.
	**/
	PipeWrite;
	/**
		Create a bidirectional pipe for IO channel.
		Both the child and the parent processes will be able to read from and to
		write into the pipe.
	**/
	PipeReadWrite;
	/**
		Use the corresponding IO stream of the parent process.
		For example if `Inherit` is used for stdin of the child process, then stdin
		of the parent process will be used.
	**/
	Inherit;
	/**
		Connect IO channel to `/dev/null` on unix-like systems and to `NUL` on windows.
	**/
	Ignore;
	/**
		Use specified file as a source and/or a target for IO.
	**/
	File(path:FilePath, flags:FileOpenFlag<Dynamic>);
	/**
		Use an opened file as a source and/or a target for IO.
	**/
	OpenedFile(file:File);
}