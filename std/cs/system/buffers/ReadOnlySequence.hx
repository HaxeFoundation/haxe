package cs.system.buffers;

@:native("System.Buffers.ReadOnlySequence")
extern class ReadOnlySequence<T> extends cs.system.ValueType {
	static var Empty(default, never):cs.system.buffers.ReadOnlySequence<Dynamic>;
	var End(default, never):cs.system.SequencePosition;
	var First(default, never):cs.system.ReadOnlyMemory<T>;
	var FirstSpan(default, never):cs.system.ReadOnlySpan<T>;
	var IsEmpty(default, never):Bool;
	var IsSingleSegment(default, never):Bool;
	var Length(default, never):haxe.Int64;
	var Start(default, never):cs.system.SequencePosition;
	@:overload(function(memory:cs.system.ReadOnlyMemory<T>):Void {})
	@:overload(function(array:cs.NativeArray<T>):Void {})
	@:overload(function(array:cs.NativeArray<T>, start:Int, length:Int):Void {})
	function new(startSegment:cs.system.buffers.ReadOnlySequenceSegment<T>, startIndex:Int, endSegment:cs.system.buffers.ReadOnlySequenceSegment<T>, endIndex:Int):Void;
	function GetEnumerator():cs.system.buffers.ReadOnlySequence_Enumerator<T>;
	@:overload(function(offset:haxe.Int64):cs.system.SequencePosition {})
	function GetPosition(offset:haxe.Int64, origin:cs.system.SequencePosition):cs.system.SequencePosition;
	@:overload(function(start:haxe.Int64):cs.system.buffers.ReadOnlySequence<T> {})
	@:overload(function(start:cs.system.SequencePosition):cs.system.buffers.ReadOnlySequence<T> {})
	@:overload(function(start:Int, length:Int):cs.system.buffers.ReadOnlySequence<T> {})
	@:overload(function(start:Int, end:cs.system.SequencePosition):cs.system.buffers.ReadOnlySequence<T> {})
	@:overload(function(start:haxe.Int64, length:haxe.Int64):cs.system.buffers.ReadOnlySequence<T> {})
	@:overload(function(start:haxe.Int64, end:cs.system.SequencePosition):cs.system.buffers.ReadOnlySequence<T> {})
	@:overload(function(start:cs.system.SequencePosition, length:Int):cs.system.buffers.ReadOnlySequence<T> {})
	@:overload(function(start:cs.system.SequencePosition, length:haxe.Int64):cs.system.buffers.ReadOnlySequence<T> {})
	function Slice(start:cs.system.SequencePosition, end:cs.system.SequencePosition):cs.system.buffers.ReadOnlySequence<T>;
	function ToString():String;
	function TryGet(position:cs.Ref<cs.system.SequencePosition>, memory:cs.Ref<cs.system.ReadOnlyMemory<T>>, ?advance:Bool):Bool;
}
