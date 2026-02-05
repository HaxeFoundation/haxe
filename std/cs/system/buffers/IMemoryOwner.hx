package cs.system.buffers;

@:native("System.Buffers.IMemoryOwner")
extern interface IMemoryOwner<T> extends cs.system.IDisposable {
	var Memory(default, never):cs.system.Memory<T>;
}
