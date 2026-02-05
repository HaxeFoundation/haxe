package cs.system.buffers;

@:native("System.Buffers.SequenceReaderExtensions")
extern class SequenceReaderExtensions {
	@:overload(function(reader:cs.Ref<cs.system.buffers.SequenceReader<cs.UInt8>>, value:cs.Ref<cs.Int16>):Bool {})
	@:overload(function(reader:cs.Ref<cs.system.buffers.SequenceReader<cs.UInt8>>, value:cs.Ref<Int>):Bool {})
	static function TryReadBigEndian(reader:cs.Ref<cs.system.buffers.SequenceReader<cs.UInt8>>, value:cs.Ref<haxe.Int64>):Bool;
	@:overload(function(reader:cs.Ref<cs.system.buffers.SequenceReader<cs.UInt8>>, value:cs.Ref<cs.Int16>):Bool {})
	@:overload(function(reader:cs.Ref<cs.system.buffers.SequenceReader<cs.UInt8>>, value:cs.Ref<Int>):Bool {})
	static function TryReadLittleEndian(reader:cs.Ref<cs.system.buffers.SequenceReader<cs.UInt8>>, value:cs.Ref<haxe.Int64>):Bool;
}
