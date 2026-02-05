package cs.system.runtime.interopservices;

/** Provides a controlled memory buffer that can be used for reading and writing. Attempts to access memory outside the controlled buffer (underruns and overruns) raise exceptions. */
@:native("System.Runtime.InteropServices.SafeBuffer")
extern class SafeBuffer extends cs.microsoft.win32.safehandles.SafeHandleZeroOrMinusOneIsInvalid {
	/**
	 * Gets the size of the buffer, in bytes.
	 * @return The number of bytes in the memory buffer.
	 */
	var ByteLength(default, never):cs.UInt64;
	/**
	 * Obtains a pointer from a  object for a block of memory.
	 * @param pointer A byte pointer, passed by reference, to receive the pointer from
	 * within the  object. You must set this pointer to  before you call this method.
	 */
	function AcquirePointer(pointer:cs.Ref<cs.Pointer<cs.UInt8>>):Void;
	@:overload(function(numBytes:cs.UInt64):Void {})
	@:overload(function<T>(numElements:cs.UInt):Void {})
	/**
	 * Specifies the allocation size of the memory buffer by using the specified number
	 * of elements and element size. You must call this method before you use the 
	 * instance.
	 * @param numElements The number of elements in the buffer.
	 * @param sizeOfEachElement The size of each element in the buffer.
	 */
	function Initialize(numElements:cs.UInt, sizeOfEachElement:cs.UInt):Void;
	/**
	 * Reads a value type from memory at the specified offset.
	 * @param T The value type to read.
	 * @param byteOffset The location from which to read the value type. You may have
	 * to consider alignment issues.
	 * @return The value type that was read from memory.
	 */
	function Read<T>(byteOffset:cs.UInt64):T;
	/**
	 * Reads the specified number of value types from memory starting at the offset,
	 * and writes them into an array starting at the index.
	 * @param T The value type to read.
	 * @param byteOffset The location from which to start reading.
	 * @param array The output array to write to.
	 * @param index The location in the output array to begin writing to.
	 * @param count The number of value types to read from the input array and to write
	 * to the output array.
	 */
	function ReadArray<T>(byteOffset:cs.UInt64, array:cs.NativeArray<T>, index:Int, count:Int):Void;
	/** Releases a pointer that was obtained by the  method. */
	function ReleasePointer():Void;
	/**
	 * Writes a value type to memory at the given location.
	 * @param T The value type to write.
	 * @param byteOffset The location at which to start writing. You may have to
	 * consider alignment issues.
	 * @param value The value to write.
	 */
	function Write<T>(byteOffset:cs.UInt64, value:T):Void;
	/**
	 * Writes the specified number of value types to a memory location by reading bytes
	 * starting from the specified location in the input array.
	 * @param T The value type to write.
	 * @param byteOffset The location in memory to write to.
	 * @param array The input array.
	 * @param index The offset in the array to start reading from.
	 * @param count The number of value types to write.
	 */
	function WriteArray<T>(byteOffset:cs.UInt64, array:cs.NativeArray<T>, index:Int, count:Int):Void;
}
