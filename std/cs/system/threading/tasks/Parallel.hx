package cs.system.threading.tasks;

/** Provides support for parallel loops and regions. */
@:native("System.Threading.Tasks.Parallel")
extern class Parallel {
	@:overload(function(fromInclusive:Int, toExclusive:Int, body:cs.system.Action_2<Int, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function(fromInclusive:Int, toExclusive:Int, body:cs.system.Action_1<Int>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function(fromInclusive:haxe.Int64, toExclusive:haxe.Int64, body:cs.system.Action_2<haxe.Int64, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function(fromInclusive:haxe.Int64, toExclusive:haxe.Int64, body:cs.system.Action_1<haxe.Int64>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function(fromInclusive:Int, toExclusive:Int, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_2<Int, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function(fromInclusive:Int, toExclusive:Int, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_1<Int>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function(fromInclusive:haxe.Int64, toExclusive:haxe.Int64, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_2<haxe.Int64, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function(fromInclusive:haxe.Int64, toExclusive:haxe.Int64, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_1<haxe.Int64>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TLocal>(fromInclusive:Int, toExclusive:Int, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<Int, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TLocal>(fromInclusive:haxe.Int64, toExclusive:haxe.Int64, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<haxe.Int64, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TLocal>(fromInclusive:Int, toExclusive:Int, parallelOptions:cs.system.threading.tasks.ParallelOptions, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<Int, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	/**
	 * Executes a  loop in which iterations may run in parallel and the state of the
	 * loop can be monitored and manipulated.
	 * @param fromInclusive The start index, inclusive.
	 * @param toExclusive The end index, exclusive.
	 * @param body The delegate that is invoked once per iteration.
	 * @return A  structure that contains information about which portion of the loop
	 * completed.
	 */
	static function For<TLocal>(fromInclusive:haxe.Int64, toExclusive:haxe.Int64, parallelOptions:cs.system.threading.tasks.ParallelOptions, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<haxe.Int64, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult;
	@:overload(function<TSource>(source:cs.system.collections.concurrent.OrderablePartitioner<TSource>, body:cs.system.Action_3<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.concurrent.Partitioner_1<TSource>, body:cs.system.Action_2<TSource, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.concurrent.Partitioner_1<TSource>, body:cs.system.Action_1<TSource>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, body:cs.system.Action_3<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, body:cs.system.Action_2<TSource, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, body:cs.system.Action_1<TSource>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.concurrent.OrderablePartitioner<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_3<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.concurrent.Partitioner_1<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_2<TSource, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.concurrent.Partitioner_1<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_1<TSource>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_3<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_2<TSource, cs.system.threading.tasks.ParallelLoopState>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, body:cs.system.Action_1<TSource>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource, TLocal>(source:cs.system.collections.concurrent.OrderablePartitioner<TSource>, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_5<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource, TLocal>(source:cs.system.collections.concurrent.Partitioner_1<TSource>, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<TSource, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource, TLocal>(source:cs.system.collections.generic.IEnumerable<TSource>, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_5<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource, TLocal>(source:cs.system.collections.generic.IEnumerable<TSource>, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<TSource, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource, TLocal>(source:cs.system.collections.concurrent.OrderablePartitioner<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_5<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource, TLocal>(source:cs.system.collections.concurrent.Partitioner_1<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<TSource, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	@:overload(function<TSource, TLocal>(source:cs.system.collections.generic.IEnumerable<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_5<TSource, cs.system.threading.tasks.ParallelLoopState, haxe.Int64, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult {})
	/**
	 * Executes a  ( in Visual Basic) operation on a  in which iterations may run in
	 * parallel and the state of the loop can be monitored and manipulated.
	 * @param TSource The type of the elements in .
	 * @param source The orderable partitioner that contains the original data source.
	 * @param body The delegate that is invoked once per iteration.
	 * @return A structure that contains information about which portion of the loop
	 * completed.
	 */
	static function ForEach<TSource, TLocal>(source:cs.system.collections.generic.IEnumerable<TSource>, parallelOptions:cs.system.threading.tasks.ParallelOptions, localInit:cs.system.Func_1<TLocal>, body:cs.system.Func_4<TSource, cs.system.threading.tasks.ParallelLoopState, TLocal, TLocal>, localFinally:cs.system.Action_1<TLocal>):cs.system.threading.tasks.ParallelLoopResult;
	@:overload(function(actions:cs.NativeArray<cs.system.Action>):Void {})
	/**
	 * Executes each of the provided actions, possibly in parallel.
	 * @param actions An array of  to execute.
	 */
	static function Invoke(parallelOptions:cs.system.threading.tasks.ParallelOptions, actions:cs.NativeArray<cs.system.Action>):Void;
}
