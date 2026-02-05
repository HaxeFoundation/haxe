package cs.system.buffers.text;

/** Provides static methods to parse Utf8 strings to common data types. */
@:native("System.Buffers.Text.Utf8Parser")
extern class Utf8Parser {
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<Bool>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt8>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.system.DateTime>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.system.DateTimeOffset>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.system.Decimal>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<Float>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.system.Guid>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.Int16>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<Int>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<haxe.Int64>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.Int8>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<Single>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.system.TimeSpan>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt16>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool {})
	/**
	 * Parses a  at the start of a Utf8 string.
	 * @param source The Utf8 string to parse.
	 * @param value When the method returns, contains the value parsed from , if the
	 * parsing operation succeeded.
	 * @param bytesConsumed If the parsing operation was successful, contains the
	 * length in bytes of the parsed substring when the method returns. If the method
	 * fails,  is set to 0.
	 * @param standardFormat The expected format of the Utf8 string.
	 * @return for success;  if the string was not syntactically valid or an overflow
	 * or underflow occurred.
	 */
	static function TryParse(source:cs.system.ReadOnlySpan<cs.UInt8>, value:cs.Ref<cs.UInt64>, bytesConsumed:cs.Ref<Int>, ?standardFormat:cs.Char16):Bool;
}
