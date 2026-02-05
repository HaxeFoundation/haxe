package cs.system.io.compression;

@:native("System.IO.Compression.BrotliDecoder")
extern class BrotliDecoder extends cs.system.ValueType {
	/**
	 * @param source 
	 * @param destination 
	 * @param bytesWritten 
	 */
	static function TryDecompress(source:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param source 
	 * @param destination 
	 * @param bytesConsumed 
	 * @param bytesWritten 
	 */
	function Decompress(source:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesConsumed:cs.Ref<Int>, bytesWritten:cs.Ref<Int>):cs.system.buffers.OperationStatus;
	function Dispose():Void;
}
