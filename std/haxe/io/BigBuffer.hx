package haxe.io;

import haxe.exceptions.NotImplementedException;

enum abstract Endian(Int) {
	var BigEndian;
	var LittleEndian;
}

/**
	TODO:
	This is an attempt to design a cross-platform API for big byte buffers (more than 2GB)
	without any unnecessary allocations.
**/
class BigBuffer {
	/**
		Current byte order for reading and writing numbers.
	**/
	public var endian(get,set):Endian;
	function get_endian():Endian throw new NotImplementedException();
	function set_endian(v:Endian):Endian throw new NotImplementedException();

	/**
		Buffer size (amount of bytes).
	**/
	public function getLength():Int64 {
		throw new NotImplementedException();
	}

	/**
		Move internal pointer to the beginning - to the byte at index 0.
	**/
	public function rewind():Void {
		throw new NotImplementedException();
	}

	/**
		Move internal pointer past the last byte.
	**/
	public function fastForward():Void {
		throw new NotImplementedException();
	}

	/**
		Move internal pointer by `step` bytes forward (if `step` is positive)
		or backward (if `step` is negative)
	**/
	public function movePointer(step:Int):Void {
		throw new NotImplementedException();
	}

	/**
		Copy up to `length` bytes from this buffer starting at the internal
		pointer position into `buffer` starting at `offset`.

		Returns amount of bytes copied.

		Advances internal pointer by the return value.
	**/
	public function copyTo(buffer:Bytes, offset:Int, length:Int):Int {
		throw new NotImplementedException();
	}

	/**
		Copy up to `length` bytes from `buffer` starting at `offset` into this
		buffer starting at the internal pointer position.

		Returns amount of bytes copied.

		Advances internal pointer by the return value.
	**/
	public function copyFrom(buffer:Bytes, offset:Int, length:Int):Int {
		throw new NotImplementedException();
	}

	/**
		Sets up to `length` consecutive bytes starting from internal pointer position
		to `value`.

		Returns amount of bytes filled.

		Advances internal pointer by the return value.
	**/
	public function fill(length:Int, value:Int):Int {
		throw new NotImplementedException();
	}

	/**
		Returns a new `Bytes` instance that contains a copy of up to `length` bytes of
		`this` instance, starting at the internal pointer position.

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by the amount of bytes returned.
	**/
	public function slice(length:Int):Bytes {
		throw new NotImplementedException();
	}

	/**
		Returns the IEEE double-precision value at the internal pointer position.

		Throws if internal pointer is less than 8 bytes to the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function getDouble():Float {
		throw new NotImplementedException();
	}

	/**
		Returns the IEEE single-precision value at the internal pointer position.

		Throws if internal pointer is less than 8 bytes to the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function getFloat():Float {
		throw new NotImplementedException();
	}

	/**
		Stores the given IEEE double-precision value `value` at the internal pointer
		position.

		Throws if internal pointer is less than 8 bytes to the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function setDouble(value:Float):Void {
		throw new NotImplementedException();
	}

	/**
		Stores the given IEEE single-precision value `value` at the internal pointer
		position.

		Throws if internal pointer is less than 8 bytes to the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function setFloat(value:Float):Void {
		throw new NotImplementedException();
	}

	/**
		Returns the 8-bit unsigned integer at the internal pointer position.

		Throws if internal pointer is at the end of this buffer.

		Advances internal pointer by 1 byte.
	**/
	public function getByte():Int {
		throw new NotImplementedException();
	}

	/**
		Stores the given 8-bit unsigned integer `value` at the internal pointer position.

		Throws if internal pointer is at the end of this buffer.
		Throws if `value` overflows 8-bit unsigned integer.

		Advances internal pointer by 1 byte.
	**/
	public function setByte(value:Int):Void {
		throw new NotImplementedException();
	}

	/**
		Returns the 16-bit unsigned integer at the internal pointer position (in
		little-endian encoding).

		Throws if internal pointer is less than 2 bytes to the end of this buffer.

		Advances internal pointer by 2 bytes.
	**/
	public function getUInt16():Int {
		throw new NotImplementedException();
	}

	/**
		Stores the given 16-bit unsigned integer `value` at the internal pointer
		position (in little-endian encoding).

		Throws if internal pointer is less than 2 bytes to the end of this buffer.
		Throws if `value` overflows 16-bit unsigned integer.

		Advances internal pointer by 2 bytes.
	**/
	public function setUInt16(value:Int):Void {
		throw new NotImplementedException();
	}

	/**
		Returns the 32-bit integer at the internal pointer position (in little-endian
		encoding).

		Throws if internal pointer is less than 4 bytes to the end of this buffer.

		Advances internal pointer by 4 bytes.
	**/
	public function getInt32():Int {
		throw new NotImplementedException();
	}

	/**
		Returns the 64-bit integer at the internal pointer position (in little-endian
		encoding).

		Throws if internal pointer is less than 8 bytes to the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function getInt64():Int64 {
		throw new NotImplementedException();
	}

	/**
		Stores the given 32-bit integer `v` at the internal pointer position (in
		little-endian encoding).

		Throws if internal pointer is less than 4 bytes to the end of this buffer.
		Throws if `value` overflows 32-bit signed integer.

		Advances internal pointer by 4 bytes.
	**/
	public function setInt32(value:Int):Void {
		throw new NotImplementedException();
	}

	/**
		Stores the given 64-bit integer `v` at the internal pointer position (in
		little-endian encoding).

		Throws if internal pointer is less than 8 bytes to the end of this buffer.

		Advances internal pointer by 8 bytes.
	**/
	public function setInt64(v:Int64):Void {
		throw new NotImplementedException();
	}

	/**
		Returns the `length`-bytes long string stored at the internal pointer position,
		interpreted with the given `encoding` (UTF-8 by default).

		Throws if internal pointer is less than `length` bytes to the end of this buffer.
		Throws if the requested bytes don't represent a valid encoded string.

		Advances internal pointer by `length` bytes.
	**/
	public function getString(length:Int, ?encoding:Encoding):String {
		throw new NotImplementedException();
	}

	public function toString():String {
		return '[BigBuffer]';
	}

	/**
		Returns a new `BigBuffer` instance with the given `length`. The values of the
		bytes are not initialized and may not be zero.
	**/
	public static function alloc(length:Int64, endian:Endian = LittleEndian):BigBuffer {
		throw new NotImplementedException();
	}

	/**
		Join `bytes` into one big buffer.

		Total length of the result buffer always equals the sum of `bytes` lengths.
	**/
	public static function join(bytes:Array<Bytes>, endian:Endian = LittleEndian):BigBuffer {
		throw new NotImplementedException();
	}
}
