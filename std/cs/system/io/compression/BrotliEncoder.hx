package cs.system.io.compression;

@:native("System.IO.Compression.BrotliEncoder")
extern class BrotliEncoder extends cs.system.ValueType {
	function new(quality:Int, window:Int):Void;
	/** @param inputSize  */
	static function GetMaxCompressedLength(inputSize:Int):Int;
	@:overload(function(source:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool {})
	/**
	 * @param source 
	 * @param destination 
	 * @param bytesWritten 
	 */
	static function TryCompress(source:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, quality:Int, window:Int):Bool;
	/**
	 * @param source 
	 * @param destination 
	 * @param bytesConsumed 
	 * @param bytesWritten 
	 * @param isFinalBlock 
	 */
	function Compress(source:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesConsumed:cs.Ref<Int>, bytesWritten:cs.Ref<Int>, isFinalBlock:Bool):cs.system.buffers.OperationStatus;
	function Dispose():Void;
	/**
	 * @param destination 
	 * @param bytesWritten 
	 */
	function Flush(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):cs.system.buffers.OperationStatus;
}
