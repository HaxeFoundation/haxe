package cs.system.collections.concurrent;

@:native("System.Collections.Concurrent.IProducerConsumerCollection")
extern interface IProducerConsumerCollection<T> extends cs.system.collections.generic.IEnumerable<T> extends cs.system.collections.IEnumerable extends cs.system.collections.ICollection {
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function ToArray():cs.NativeArray<T>;
	function TryAdd(item:T):Bool;
	function TryTake(item:cs.Ref<T>):Bool;
}
