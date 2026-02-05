package cs.system.buffers.text;

/** Provides static mthods to format common data types as Utf8 strings. */
@:native("System.Buffers.Text.Utf8Formatter")
extern class Utf8Formatter {
	@:overload(function(value:Bool, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.UInt8, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.system.DateTime, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.system.DateTimeOffset, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.system.Decimal, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:Float, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.system.Guid, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.Int16, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:Int, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:haxe.Int64, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.Int8, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:Single, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.system.TimeSpan, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.UInt16, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	@:overload(function(value:cs.UInt, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool {})
	/**
	 * Formats a  as a UTF8 string.
	 * @param value The value to format.
	 * @param destination The buffer to write the UTF8-formatted value to.
	 * @param bytesWritten When the method returns, contains the length of the
	 * formatted text in bytes.
	 * @param format The standard format to use.
	 * @return if the formatting operation succeeds;  if  is too small.
	 */
	static function TryFormat(value:cs.UInt64, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?format:cs.system.buffers.StandardFormat):Bool;
}
