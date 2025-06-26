package eval.luv;

import haxe.io.Bytes;

/**
	Data buffers.

	@see https://aantron.github.io/luv/luv/Luv/Buffer
**/
@:coreType abstract Buffer {
	/**
		Allocates a fresh buffer of the given size.
	**/
	static public function create(size:Int):Buffer;

	/**
		Creates a buffer from a string.
	**/
	@:from static public function fromNativeString(s:NativeString):Buffer;

	/**
		Creates a buffer from a string.
	**/
	@:from static public function fromString(s:String):Buffer;

	/**
		Creates a buffer from bytes.
	**/
	@:from static public function fromBytes(b:Bytes):Buffer;

	/**
		Evaluates to the sum of the sizes of the buffers in the array.
	**/
	static public function totalSize(buffers:Array<Buffer>):Int;

	/**
		`Buffer.drop(buffers, count)` drops the first `count` bytes from `buffers`.

		For example, if `buffers` contains two buffers of size 16, `Buffer.drop(buffers, 18)`
		will evaluate to an array that has lost the reference to the first buffer,
		and contains only a view into the second buffer of size 14.
	**/
	static public function drop(buffers:Array<Buffer>, count:Int):Array<Buffer>;

	/**
		Evaluates to the size of the buffer.
	**/
	public function size():Int;

	/**
		Retrieve a byte at the given index.
	**/
	@:arrayAccess public function get(index:Int):Int;

	/**
		Retrieve a byte at the given index without a bounds check.
	**/
	public function unsafeGet(index:Int):Int;

	/**
		Set byte value at the given index.
	**/
	@:arrayAccess public function set(index:Int, byte:Int):Int;

	/**
		Set byte value at the given index without a bounds check.
	**/
	public function unsafeSet(index:Int, byte:Int):Int;

	/**
		Creates a view into buffer that starts at the given `offset` and has the given `length`.

		No data is copied.
	**/
	public function sub(offset:Int, length:Int):Buffer;

	/**
		Copies data from this buffer to destination.

		The amount of data copied is the minimum of the two buffers' size.
	**/
	public function blit(destination:Buffer):Void;

	/**
		Fills the given buffer with the given byte.
	**/
	public function fill(byte:Int):Void;

	/**
		Creates a string with the same contents as the buffer.
	**/
	public function toString():String;

	/**
		Creates a native string with the same contents as the buffer.
	**/
	public function toNativeString():NativeString;

	/**
		Creates a `haxe.io.Bytes` instance with the same contents as this buffer.
	**/
	public function toBytes():Bytes;

	/**
		Copies data from a buffer to bytes buffer.
	**/
	public function blitToBytes(destination:Bytes, destinationOffset:Int):Void;

	/**
		Copies data from bytes to a buffer.
	**/
	public function blitFromBytes(source:Bytes, sourceOffset:Int):Void;

	/**
		Copies data from bytes to a buffer.

		Note: `sourceOffset` is not a character offset but a byte offset.
	**/
	public function blitFromString(source:NativeString, sourceOffset:Int):Void;
}