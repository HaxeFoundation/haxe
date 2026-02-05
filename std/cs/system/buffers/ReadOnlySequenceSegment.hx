package cs.system.buffers;

@:native("System.Buffers.ReadOnlySequenceSegment")
extern class ReadOnlySequenceSegment<T> {
	var Memory(default, default):cs.system.ReadOnlyMemory<T>;
	var Next(default, default):cs.system.buffers.ReadOnlySequenceSegment<T>;
	var RunningIndex(default, default):haxe.Int64;
}
