package asyncio;

import asyncio.IDuplex;
import asyncio.IReadable;
import asyncio.IWritable;

/**
	Enum which is used to stay type-safe when a stream can be of any type.
**/
enum Stream {
	/** Read-only stream. */
	Read(stream:IReadable);
	/** Write-only stream. */
	Write(stream:IWritable);
	/** The stream is both readable and writable. */
	ReadWrite(stream:IDuplex);
}