package cs.system.buffers;

@:native("System.Buffers.SequenceReader")
extern class SequenceReader<T> extends cs.system.ValueType {
	var Consumed(default, never):haxe.Int64;
	var CurrentSpan(default, never):cs.system.ReadOnlySpan<T>;
	var CurrentSpanIndex(default, never):Int;
	var End(default, never):Bool;
	var Length(default, never):haxe.Int64;
	var Position(default, never):cs.system.SequencePosition;
	var Remaining(default, never):haxe.Int64;
	var Sequence(default, never):cs.system.buffers.ReadOnlySequence<T>;
	var UnreadSpan(default, never):cs.system.ReadOnlySpan<T>;
	function new(sequence:cs.system.buffers.ReadOnlySequence<T>):Void;
	function Advance(count:haxe.Int64):Void;
	function AdvancePast(value:T):haxe.Int64;
	@:overload(function(values:cs.system.ReadOnlySpan<T>):haxe.Int64 {})
	@:overload(function(value0:T, value1:T):haxe.Int64 {})
	@:overload(function(value0:T, value1:T, value2:T):haxe.Int64 {})
	function AdvancePastAny(value0:T, value1:T, value2:T, value3:T):haxe.Int64;
	@:overload(function(next:cs.system.ReadOnlySpan<T>, ?advancePast:Bool):Bool {})
	function IsNext(next:T, ?advancePast:Bool):Bool;
	function Rewind(count:haxe.Int64):Void;
	function TryAdvanceTo(delimiter:T, ?advancePastDelimiter:Bool):Bool;
	function TryAdvanceToAny(delimiters:cs.system.ReadOnlySpan<T>, ?advancePastDelimiter:Bool):Bool;
	function TryCopyTo(destination:cs.system.Span<T>):Bool;
	function TryPeek(value:cs.Ref<T>):Bool;
	function TryRead(value:cs.Ref<T>):Bool;
	@:overload(function(sequence:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>, delimiter:cs.system.ReadOnlySpan<T>, ?advancePastDelimiter:Bool):Bool {})
	@:overload(function(sequence:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>, delimiter:T, ?advancePastDelimiter:Bool):Bool {})
	@:overload(function(span:cs.Ref<cs.system.ReadOnlySpan<T>>, delimiter:T, ?advancePastDelimiter:Bool):Bool {})
	@:overload(function(sequence:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>, delimiter:T, delimiterEscape:T, ?advancePastDelimiter:Bool):Bool {})
	function TryReadTo(span:cs.Ref<cs.system.ReadOnlySpan<T>>, delimiter:T, delimiterEscape:T, ?advancePastDelimiter:Bool):Bool;
	@:overload(function(sequence:cs.Ref<cs.system.buffers.ReadOnlySequence<T>>, delimiters:cs.system.ReadOnlySpan<T>, ?advancePastDelimiter:Bool):Bool {})
	function TryReadToAny(span:cs.Ref<cs.system.ReadOnlySpan<T>>, delimiters:cs.system.ReadOnlySpan<T>, ?advancePastDelimiter:Bool):Bool;
}
