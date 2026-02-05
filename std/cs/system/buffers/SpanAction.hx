package cs.system.buffers;

@:native("System.Buffers.SpanAction")
extern class SpanAction<T, TArg> extends cs.system.MulticastDelegate {
	function new(func:(span:cs.system.Span<T>, arg:TArg)->Void):Void;
	function Invoke(span:cs.system.Span<T>, arg:TArg):Void;
}
