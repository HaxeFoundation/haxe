package cs.system.buffers;

@:native("System.Buffers.ReadOnlySequence`1.Enumerator")
extern class ReadOnlySequence_Enumerator<T> extends cs.system.ValueType {
	var Current(default, never):cs.system.ReadOnlyMemory<T>;
	function new(sequence:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>):Void;
	function MoveNext():Bool;
}
