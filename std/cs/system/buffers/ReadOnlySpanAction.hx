package cs.system.buffers;

@:native("System.Buffers.ReadOnlySpanAction")
extern class ReadOnlySpanAction<T, TArg> extends cs.system.MulticastDelegate {
	function new(func:(span:cs.system.ReadOnlySpan<T>, arg:TArg)->Void):Void;
	function Invoke(span:cs.system.ReadOnlySpan<T>, arg:TArg):Void;
}
