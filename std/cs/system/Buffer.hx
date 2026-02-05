package cs.system;

/** Manipulates arrays of primitive types. */
@:native("System.Buffer")
extern class Buffer {
	/**
	 * Copies a specified number of bytes from a source array starting at a particular
	 * offset to a destination array starting at a particular offset.
	 * @param src The source buffer.
	 * @param srcOffset The zero-based byte offset into .
	 * @param dst The destination buffer.
	 * @param dstOffset The zero-based byte offset into .
	 * @param count The number of bytes to copy.
	 */
	static function BlockCopy(src:cs.system.Array, srcOffset:Int, dst:cs.system.Array, dstOffset:Int, count:Int):Void;
	/**
	 * Returns the number of bytes in the specified array.
	 * @param array An array.
	 * @return The number of bytes in the array.
	 */
	static function ByteLength(array:cs.system.Array):Int;
	/**
	 * Retrieves the byte at the specified location in the specified array.
	 * @param array An array.
	 * @param index A location in the array.
	 * @return The byte at the specified location in the array.
	 */
	static function GetByte(array:cs.system.Array, index:Int):cs.UInt8;
	@:overload(function(source:cs.Pointer<Void>, destination:cs.Pointer<Void>, destinationSizeInBytes:haxe.Int64, sourceBytesToCopy:haxe.Int64):Void {})
	/**
	 * Copies a number of bytes specified as a long integer value from one address in
	 * memory to another. This API is not CLS-compliant.
	 * @param source The address of the bytes to copy.
	 * @param destination The target address.
	 * @param destinationSizeInBytes The number of bytes available in the destination
	 * memory block.
	 * @param sourceBytesToCopy The number of bytes to copy.
	 */
	static function MemoryCopy(source:cs.Pointer<Void>, destination:cs.Pointer<Void>, destinationSizeInBytes:cs.UInt64, sourceBytesToCopy:cs.UInt64):Void;
	/**
	 * Assigns a specified value to a byte at a particular location in a specified
	 * array.
	 * @param array An array.
	 * @param index A location in the array.
	 * @param value A value to assign.
	 */
	static function SetByte(array:cs.system.Array, index:Int, value:cs.UInt8):Void;
}
