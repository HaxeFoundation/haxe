package cs.system.buffers.binary;

/** Reads bytes as primitives with specific endianness. */
@:native("System.Buffers.Binary.BinaryPrimitives")
extern class BinaryPrimitives {
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span to read.
	 * @return The big endian value.
	 */
	static function ReadInt16BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.Int16;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span to read.
	 * @return The little endian value.
	 */
	static function ReadInt16LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.Int16;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span to read.
	 * @return The big endian value.
	 */
	static function ReadInt32BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):Int;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span to read.
	 * @return The little endian value.
	 */
	static function ReadInt32LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):Int;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span to read.
	 * @return The big endian value.
	 */
	static function ReadInt64BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):haxe.Int64;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span to read.
	 * @return The little endian value.
	 */
	static function ReadInt64LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):haxe.Int64;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span to read.
	 * @return The big endian value.
	 */
	static function ReadUInt16BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt16;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span to read.
	 * @return The little endian value.
	 */
	static function ReadUInt16LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt16;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span to read.
	 * @return The big endian value.
	 */
	static function ReadUInt32BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @return The little endian value.
	 */
	static function ReadUInt32LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span of bytes to read.
	 * @return The big endian value.
	 */
	static function ReadUInt64BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt64;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @return The little endian value.
	 */
	static function ReadUInt64LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>):cs.UInt64;
	@:overload(function(value:cs.UInt8):cs.UInt8 {})
	@:overload(function(value:cs.Int16):cs.Int16 {})
	@:overload(function(value:Int):Int {})
	@:overload(function(value:haxe.Int64):haxe.Int64 {})
	@:overload(function(value:cs.Int8):cs.Int8 {})
	@:overload(function(value:cs.UInt16):cs.UInt16 {})
	@:overload(function(value:cs.UInt):cs.UInt {})
	/**
	 * Reverses a primitive value by performing an endianness swap of the specified 
	 * value, which effectively does nothing for a .
	 * @param value The value to reverse.
	 * @return The passed-in value, unmodified.
	 */
	static function ReverseEndianness(value:cs.UInt64):cs.UInt64;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as big endian.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryReadInt16BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.Int16>):Bool;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as little endian.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryReadInt16LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.Int16>):Bool;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as big endian.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryReadInt32BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<Int>):Bool;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as little endian.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryReadInt32LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<Int>):Bool;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as big endian.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryReadInt64BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<haxe.Int64>):Bool;
	/**
	 * Reads an  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as little endian.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryReadInt64LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<haxe.Int64>):Bool;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as big endian.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryReadUInt16BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt16>):Bool;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as little endian.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryReadUInt16LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt16>):Bool;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as big endian.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryReadUInt32BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt>):Bool;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as little endian.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryReadUInt32LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt>):Bool;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as big endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as big endian.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryReadUInt64BigEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt64>):Bool;
	/**
	 * Reads a  from the beginning of a read-only span of bytes, as little endian.
	 * @param source The read-only span of bytes to read.
	 * @param value When this method returns, the value read out of the read-only span
	 * of bytes, as little endian.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryReadUInt64LittleEndian(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt64>):Bool;
	/**
	 * Writes an  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryWriteInt16BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.Int16):Bool;
	/**
	 * Writes an  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryWriteInt16LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.Int16):Bool;
	/**
	 * Writes an  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryWriteInt32BigEndian(destination:cs.system.Span<cs.UInt8>, value:Int):Bool;
	/**
	 * Writes an  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryWriteInt32LittleEndian(destination:cs.system.Span<cs.UInt8>, value:Int):Bool;
	/**
	 * Writes an  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryWriteInt64BigEndian(destination:cs.system.Span<cs.UInt8>, value:haxe.Int64):Bool;
	/**
	 * Writes an  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain an ; otherwise, .
	 */
	static function TryWriteInt64LittleEndian(destination:cs.system.Span<cs.UInt8>, value:haxe.Int64):Bool;
	/**
	 * Writes a  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryWriteUInt16BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt16):Bool;
	/**
	 * Writes a  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryWriteUInt16LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt16):Bool;
	/**
	 * Writes a  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryWriteUInt32BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt):Bool;
	/**
	 * Writes a  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryWriteUInt32LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt):Bool;
	/**
	 * Writes a  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryWriteUInt64BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt64):Bool;
	/**
	 * Writes a  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 * @return if the span is large enough to contain a ; otherwise, .
	 */
	static function TryWriteUInt64LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt64):Bool;
	/**
	 * Writes an  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteInt16BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.Int16):Void;
	/**
	 * Writes an  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteInt16LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.Int16):Void;
	/**
	 * Writes an  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteInt32BigEndian(destination:cs.system.Span<cs.UInt8>, value:Int):Void;
	/**
	 * Writes an  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteInt32LittleEndian(destination:cs.system.Span<cs.UInt8>, value:Int):Void;
	/**
	 * Writes an  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteInt64BigEndian(destination:cs.system.Span<cs.UInt8>, value:haxe.Int64):Void;
	/**
	 * Writes an  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteInt64LittleEndian(destination:cs.system.Span<cs.UInt8>, value:haxe.Int64):Void;
	/**
	 * Writes a  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteUInt16BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt16):Void;
	/**
	 * Writes a  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteUInt16LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt16):Void;
	/**
	 * Writes a  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteUInt32BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt):Void;
	/**
	 * Writes a  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteUInt32LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt):Void;
	/**
	 * Writes a  into a span of bytes, as big endian.
	 * @param destination The span of bytes where the value is to be written, as big
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteUInt64BigEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt64):Void;
	/**
	 * Writes a  into a span of bytes, as little endian.
	 * @param destination The span of bytes where the value is to be written, as little
	 * endian.
	 * @param value The value to write into the span of bytes.
	 */
	static function WriteUInt64LittleEndian(destination:cs.system.Span<cs.UInt8>, value:cs.UInt64):Void;
}
