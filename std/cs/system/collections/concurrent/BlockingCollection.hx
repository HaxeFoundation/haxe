package cs.system.collections.concurrent;

@:native("System.Collections.Concurrent.BlockingCollection")
extern class BlockingCollection<T> {
	var BoundedCapacity(default, never):Int;
	var Count(default, never):Int;
	var IsAddingCompleted(default, never):Bool;
	var IsCompleted(default, never):Bool;
	@:overload(function():Void {})
	@:overload(function(collection:cs.system.collections.concurrent.IProducerConsumerCollection<T>):Void {})
	@:overload(function(boundedCapacity:Int):Void {})
	function new(collection:cs.system.collections.concurrent.IProducerConsumerCollection<T>, boundedCapacity:Int):Void;
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:T):Int {})
	static function AddToAny<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:T, cancellationToken:cs.system.threading.CancellationToken):Int;
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:cs.Ref<T>):Int {})
	static function TakeFromAny<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:cs.Ref<T>, cancellationToken:cs.system.threading.CancellationToken):Int;
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:T):Int {})
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:T, millisecondsTimeout:Int):Int {})
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:T, timeout:cs.system.TimeSpan):Int {})
	static function TryAddToAny<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:T, millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Int;
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:cs.Ref<T>):Int {})
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:cs.Ref<T>, millisecondsTimeout:Int):Int {})
	@:overload(function<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:cs.Ref<T>, timeout:cs.system.TimeSpan):Int {})
	static function TryTakeFromAny<T>(collections:cs.NativeArray<cs.system.collections.concurrent.BlockingCollection<T>>, item:cs.Ref<T>, millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Int;
	@:overload(function(item:T):Void {})
	function Add(item:T, cancellationToken:cs.system.threading.CancellationToken):Void;
	function CompleteAdding():Void;
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function Dispose():Void;
	@:overload(function():cs.system.collections.generic.IEnumerable<T> {})
	function GetConsumingEnumerable(cancellationToken:cs.system.threading.CancellationToken):cs.system.collections.generic.IEnumerable<T>;
	@:overload(function():T {})
	function Take(cancellationToken:cs.system.threading.CancellationToken):T;
	function ToArray():cs.NativeArray<T>;
	@:overload(function(item:T):Bool {})
	@:overload(function(item:T, millisecondsTimeout:Int):Bool {})
	@:overload(function(item:T, timeout:cs.system.TimeSpan):Bool {})
	function TryAdd(item:T, millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool;
	@:overload(function(item:cs.Ref<T>):Bool {})
	@:overload(function(item:cs.Ref<T>, millisecondsTimeout:Int):Bool {})
	@:overload(function(item:cs.Ref<T>, timeout:cs.system.TimeSpan):Bool {})
	function TryTake(item:cs.Ref<T>, millisecondsTimeout:Int, cancellationToken:cs.system.threading.CancellationToken):Bool;
}
