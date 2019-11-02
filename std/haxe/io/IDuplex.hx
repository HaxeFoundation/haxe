package haxe.io;

/**
	A stream which is both readable and writable.

	This interface should be used wherever an object that is both readable and
	writable is expected, regardless of a specific implementation. See `Duplex`
	for an abstract base class that can be used to implement an `IDuplex`.

	See also `IReadable` and `IWritable`.
**/
interface IDuplex extends IReadable extends IWritable {}
