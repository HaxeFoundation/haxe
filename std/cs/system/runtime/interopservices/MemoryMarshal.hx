package cs.system.runtime.interopservices;

/** Provides methods to interoperate with , ,  , and  . */
@:native("System.Runtime.InteropServices.MemoryMarshal")
extern class MemoryMarshal {
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>):cs.system.ReadOnlySpan<cs.UInt8> {})
	/**
	 * Casts a  of one primitive type, , to a .
	 * @param T The type of items in the read-only span.
	 * @param span The source slice to convert.
	 * @return A read-only span of type .
	 */
	static function AsBytes<T>(span:cs.system.Span<T>):cs.system.Span<cs.UInt8>;
	/**
	 * Creates a  instance from a .
	 * @param T The type of items in the read-only memory buffer.
	 * @param memory The read-only memory buffer.
	 * @return A memory block that represents the same memory as the .
	 */
	static function AsMemory<T>(memory:cs.system.ReadOnlyMemory<T>):cs.system.Memory<T>;
	@:overload(function<TFrom, TTo>(span:cs.system.ReadOnlySpan<TFrom>):cs.system.ReadOnlySpan<TTo> {})
	/**
	 * Casts a read-only span of one primitive type to a read-only span of another
	 * primitive type.
	 * @param TFrom The type of the source span.
	 * @param TTo The type of the target span.
	 * @param span The source slice to convert.
	 * @return The converted read-only span.
	 */
	static function Cast<TFrom, TTo>(span:cs.system.Span<TFrom>):cs.system.Span<TTo>;
	/**
	 * Creates a new memory buffer over the portion of the pre-pinned target array
	 * beginning at the  index and consisting of  items.
	 * @param T The type of the array.
	 * @param array The pre-pinned source array.
	 * @param start The index of  at which to begin the memory block.
	 * @param length The number of items to include in the memory block.
	 * @return A block of memory over the specified elements of . If  is , or if  and 
	 * are 0, the method returns a  instance of  zero.
	 */
	static function CreateFromPinnedArray<T>(array:cs.NativeArray<T>, start:Int, length:Int):cs.system.Memory<T>;
	/**
	 * Creates a new read-only span over a portion of a regular managed object.
	 * @param T The type of the data items.
	 * @param reference A reference to data.
	 * @param length The number of  elements that  contains.
	 * @return A read-only span.
	 */
	static function CreateReadOnlySpan<T>(reference:cs.Ref<T>, length:Int):cs.system.ReadOnlySpan<T>;
	/**
	 * Creates a new span over a portion of a regular managed object.
	 * @param T The type of the data items.
	 * @param reference A reference to data.
	 * @param length The number of  elements that  contains.
	 * @return A span.
	 */
	static function CreateSpan<T>(reference:cs.Ref<T>, length:Int):cs.system.Span<T>;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>):T {})
	/**
	 * Returns a reference to the element of the read-only span at index 0.
	 * @param T The type of items in the span.
	 * @param span The read-only span from which the reference is retrieved.
	 * @return A reference to the element at index 0.
	 */
	static function GetReference<T>(span:cs.system.Span<T>):T;
	/**
	 * Reads a structure of type  out of a read-only span of bytes.
	 * @param T The type of the item to retrieve from the read-only span.
	 * @param source A read-only span.
	 * @return The structure retrieved from the read-only span.
	 */
	static function Read<T>(source:cs.system.ReadOnlySpan<cs.UInt8>):T;
	/**
	 * Creates an  view of the given read-only memory buffer.
	 * @param T The type of the items in the read-only memory buffer.
	 * @param memory A read-only memory buffer.
	 * @return An enumerable view of .
	 */
	static function ToEnumerable<T>(memory:cs.system.ReadOnlyMemory<T>):cs.system.collections.generic.IEnumerable<T>;
	/**
	 * Tries to get an array segment from the underlying memory buffer. The return
	 * value indicates the success of the operation.
	 * @param T The type of items in the read-only memory buffer.
	 * @param memory A read-only memory buffer.
	 * @param segment When this method returns, contains the array segment retrieved
	 * from the underlying read-only memory buffer. If the method fails, the method
	 * returns a default array segment.
	 * @return if the method call succeeds;  otherwise.
	 */
	static function TryGetArray<T>(memory:cs.system.ReadOnlyMemory<T>, segment:cs.Ref<cs.system.ArraySegment<T>>):Bool;
	@:overload(function<T, TManager>(memory:cs.system.ReadOnlyMemory<T>, manager:cs.Ref<TManager>):Bool {})
	static function TryGetMemoryManager<T, TManager>(memory:cs.system.ReadOnlyMemory<T>, manager:cs.Ref<TManager>, start:cs.Ref<Int>, length:cs.Ref<Int>):Bool;
	/**
	 * Tries to get the underlying string from a .
	 * @param memory Read-only memory containing a block of characters.
	 * @param text When the method returns, the string contained in the memory buffer.
	 * @param start The starting location in .
	 * @param length The number of characters in .
	 * @return if the method successfully retrieves the underlying string; otherwise, .
	 */
	static function TryGetString(memory:cs.system.ReadOnlyMemory<cs.Char16>, text:cs.Ref<String>, start:cs.Ref<Int>, length:cs.Ref<Int>):Bool;
	/**
	 * Tries to read a structure of type  from a read-only span of bytes.
	 * @param T The type of the structure to retrieve.
	 * @param source A read-only span of bytes.
	 * @param value When the method returns, an instance of .
	 * @return if the method succeeds in retrieving an instance of the structure;
	 * otherwise, .
	 */
	static function TryRead<T>(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<T>):Bool;
	/**
	 * Tries to write a structure of type  into a span of bytes.
	 * @param T The type of the structure.
	 * @param destination The span of bytes to contain the structure.
	 * @param value The structure to be written to the span.
	 * @return if the write operation succeeded; otherwise, . The method returns  if
	 * the span is too small to contain .
	 */
	static function TryWrite<T>(destination:cs.system.Span<cs.UInt8>, value:cs.Ref<T>):Bool;
	/**
	 * Writes a structure of type  into a span of bytes.
	 * @param T The type of the structure.
	 * @param destination The span of bytes to contain the structure.
	 * @param value The structure to be written to the span.
	 */
	static function Write<T>(destination:cs.system.Span<cs.UInt8>, value:cs.Ref<T>):Void;
}
