package cs.system.runtime.interopservices;

/** Provides a collection of methods for interoperating with . */
@:native("System.Runtime.InteropServices.SequenceMarshal")
extern class SequenceMarshal {
	/**
	 * Gets an array segment from the underlying read-only sequence.
	 * @param T The type of the read-only sequence.
	 * @param sequence The read-only sequence from which the array segment will be
	 * retrieved.
	 * @param segment The returned array segment.
	 * @return if it's possible to retrieve the array segment; otherwise,  and a
	 * default array segment is returned.
	 */
	static function TryGetArray<T>(sequence:cs.system.buffers.ReadOnlySequence<T>, segment:cs.Ref<cs.system.ArraySegment<T>>):Bool;
	/**
	 * Attempts to retrieve a read-only memory from the specified read-only sequence.
	 * @param T The type of the read-only sequence.
	 * @param sequence The read-only sequence from which the memory will be retrieved.
	 * @param memory The returned read-only memory of type T.
	 * @return if the read-only memory can be retrieved; otherwise, .
	 */
	static function TryGetReadOnlyMemory<T>(sequence:cs.system.buffers.ReadOnlySequence<T>, memory:cs.Ref<cs.system.ReadOnlyMemory<T>>):Bool;
	/**
	 * Attempts to retrieve a read-only sequence segment from the specified read-only
	 * sequence.
	 * @param T The type of the read-only sequence.
	 * @param sequence The read-only sequence from which the read-only sequence segment
	 * will be retrieved.
	 * @param startSegment The beginning read-only sequence segment.
	 * @param startIndex The initial position.
	 * @param endSegment The ending read-only sequence segment.
	 * @param endIndex The final position.
	 * @return if the read-only sequence segment can be retrieved; otherwise, .
	 */
	static function TryGetReadOnlySequenceSegment<T>(sequence:cs.system.buffers.ReadOnlySequence<T>, startSegment:cs.Ref<cs.system.buffers.ReadOnlySequenceSegment<T>>, startIndex:cs.Ref<Int>, endSegment:cs.Ref<cs.system.buffers.ReadOnlySequenceSegment<T>>, endIndex:cs.Ref<Int>):Bool;
	static function TryRead<T>(reader:cs.Ref<cs.system.buffers.SequenceReader<cs.UInt8>>, value:cs.Ref<T>):Bool;
}
