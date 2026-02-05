package cs.system.buffers;

/** Provides extension methods for . */
@:native("System.Buffers.BuffersExtensions")
extern class BuffersExtensions {
	static function CopyTo<T>(source:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>, destination:cs.system.Span<T>):Void;
	static function PositionOf<T>(source:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>, value:T):Null<cs.system.SequencePosition>;
	static function ToArray<T>(sequence:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>):cs.NativeArray<T>;
	/**
	 * Writes the contents of  to .
	 * @param T The type of the items in the .
	 * @param writer The buffer writer to which to write .
	 * @param value The read-only span to be written to .
	 */
	static function Write<T>(writer:cs.system.buffers.IBufferWriter<T>, value:cs.system.ReadOnlySpan<T>):Void;
}
