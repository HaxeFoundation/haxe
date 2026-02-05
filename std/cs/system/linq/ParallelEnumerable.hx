package cs.system.linq;

/** Provides a set of methods for querying objects that implement ParallelQuery{TSource}. This is the parallel equivalent of . */
@:native("System.Linq.ParallelEnumerable")
extern class ParallelEnumerable {
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, func:cs.system.Func_3<TSource, TSource, TSource>):TSource {})
	@:overload(function<TSource, TAccumulate>(source:cs.system.linq.ParallelQuery_1<TSource>, seed:TAccumulate, func:cs.system.Func_3<TAccumulate, TSource, TAccumulate>):TAccumulate {})
	@:overload(function<TSource, TAccumulate, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, seed:TAccumulate, func:cs.system.Func_3<TAccumulate, TSource, TAccumulate>, resultSelector:cs.system.Func_2<TAccumulate, TResult>):TResult {})
	@:overload(function<TSource, TAccumulate, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, seedFactory:cs.system.Func_1<TAccumulate>, updateAccumulatorFunc:cs.system.Func_3<TAccumulate, TSource, TAccumulate>, combineAccumulatorsFunc:cs.system.Func_3<TAccumulate, TAccumulate, TAccumulate>, resultSelector:cs.system.Func_2<TAccumulate, TResult>):TResult {})
	/**
	 * Applies in parallel an accumulator function over a sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence to aggregate over.
	 * @param func An accumulator function to be invoked on each element.
	 * @return The final accumulator value.
	 */
	static function Aggregate<TSource, TAccumulate, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, seed:TAccumulate, updateAccumulatorFunc:cs.system.Func_3<TAccumulate, TSource, TAccumulate>, combineAccumulatorsFunc:cs.system.Func_3<TAccumulate, TAccumulate, TAccumulate>, resultSelector:cs.system.Func_2<TAccumulate, TResult>):TResult;
	/**
	 * Determines in parallel whether all elements of a sequence satisfy a condition.
	 * @param TSource The type of elements of .
	 * @param source A sequence whose elements to apply the predicate to.
	 * @param predicate A function to test each element for a condition.
	 * @return true if every element of the source sequence passes the test in the
	 * specified predicate, or if the sequence is empty; otherwise, false.
	 */
	static function All<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):Bool;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):Bool {})
	/**
	 * Determines whether a parallel sequence contains any elements.
	 * @param TSource The type of elements of .
	 * @param source The sequence to check for emptiness.
	 * @return true if the source sequence contains any elements; otherwise, false.
	 */
	static function Any<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):Bool;
	/**
	 * Converts a  into an  to force sequential evaluation of the query.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to cast as .
	 * @return The input sequence typed as .
	 */
	static function AsEnumerable<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function(source:cs.system.linq.ParallelQuery):cs.system.linq.ParallelQuery {})
	/**
	 * Enables treatment of a data source as if it were ordered, overriding the default
	 * of unordered. AsOrdered may only be invoked on non-generic sequences returned by
	 * AsParallel, ParallelEnumerable.Range, and ParallelEnumerable.Repeat.
	 * @param source The input sequence.
	 * @return The source sequence which will maintain the original ordering in the
	 * subsequent query operators.
	 */
	static function AsOrdered<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function(source:cs.system.collections.IEnumerable):cs.system.linq.ParallelQuery {})
	@:overload(function<TSource>(source:cs.system.collections.concurrent.Partitioner_1<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * Enables parallelization of a query.
	 * @param source An  to convert to a .
	 * @return The source as a ParallelQuery to bind to ParallelEnumerable extension
	 * methods.
	 */
	static function AsParallel<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	/**
	 * Converts a  into an  to force sequential evaluation of the query.
	 * @param TSource The type of elements of .
	 * @param source A  to convert to an .
	 * @return The source as an  to bind to sequential extension methods.
	 */
	static function AsSequential<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	/**
	 * Allows an intermediate query to be treated as if no ordering is implied among
	 * the elements.
	 * @param TSource The type of elements of .
	 * @param source The input sequence.
	 * @return The source sequence with arbitrary order.
	 */
	static function AsUnordered<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function(source:cs.system.linq.ParallelQuery_1<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Float>):Float {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Int>):Float {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<haxe.Int64>):Float {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Int>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<haxe.Int64>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Int>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	/**
	 * Computes in parallel the average of a sequence of values.
	 * @param source A sequence of values that are used to calculate an average.
	 * @return The average of the sequence of values.
	 */
	static function Average<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Single>):Single;
	/**
	 * Converts the elements of a ParallelQuery to the specified type.
	 * @param TResult The type to convert the elements of  to.
	 * @param source The sequence that contains the elements to be converted.
	 * @return A sequence that contains each element of the source sequence converted
	 * to the specified type.
	 */
	static function Cast<TResult>(source:cs.system.linq.ParallelQuery):cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * This Concat overload should never be called. This method is marked as obsolete
	 * and always throws  when called.
	 * @param TSource This type parameter is not used.
	 * @param first This parameter is not used.
	 * @param second This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function Concat<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, value:TSource):Bool {})
	/**
	 * Determines in parallel whether a sequence contains a specified element by using
	 * the default equality comparer.
	 * @param TSource The type of elements of .
	 * @param source A sequence in which to locate a value.
	 * @param value The value to locate in the sequence.
	 * @return true if the source sequence contains an element that has the specified
	 * value; otherwise, false.
	 */
	static function Contains<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, value:TSource, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):Bool;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):Int {})
	/**
	 * Returns the number of elements in a parallel sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence that contains elements to be counted.
	 * @return The number of elements in the input sequence.
	 */
	static function Count<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):Int;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * Returns the elements of the specified parallel sequence or the type parameter's
	 * default value in a singleton collection if the sequence is empty.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return a default value for if it is empty.
	 * @return A sequence that contains default(TSource) if  is empty; otherwise, .
	 */
	static function DefaultIfEmpty<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, defaultValue:TSource):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * Returns distinct elements from a parallel sequence by using the default equality
	 * comparer to compare values.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to remove duplicate elements from.
	 * @return A sequence that contains distinct elements from the source sequence.
	 */
	static function Distinct<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	/**
	 * Returns the element at a specified index in a parallel sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence to return an element from.
	 * @param index The zero-based index of the element to retrieve.
	 * @return The element at the specified position in the source sequence.
	 */
	static function ElementAt<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, index:Int):TSource;
	/**
	 * Returns the element at a specified index in a parallel sequence or a default
	 * value if the index is out of range.
	 * @param TSource The type of the elements of .
	 * @param source A sequence to return an element from.
	 * @param index The zero-based index of the element to retrieve.
	 * @return default(TSource) if the index is outside the bounds of the source
	 * sequence; otherwise, the element at the specified position in the source
	 * sequence.
	 */
	static function ElementAtOrDefault<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, index:Int):TSource;
	/**
	 * Returns an empty ParallelQuery{TResult} that has the specified type argument.
	 * @param TResult The type to assign to the type parameter of the returned generic
	 * sequence.
	 * @return An empty sequence whose type argument is .
	 */
	static function Empty<TResult>():cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * This Except overload should never be called. This method is marked as obsolete
	 * and always throws  when called.
	 * @param TSource This type parameter is not used.
	 * @param first This parameter is not used.
	 * @param second This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function Except<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	/**
	 * Returns the first element of a parallel sequence.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return the first element of.
	 * @return The first element in the specified sequence.
	 */
	static function First<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	/**
	 * Returns the first element of a parallel sequence, or a default value if the
	 * sequence contains no elements.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return the first element of.
	 * @return default(TSource) if  is empty; otherwise, the first element in .
	 */
	static function FirstOrDefault<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	/**
	 * Invokes in parallel the specified action for each element in the .
	 * @param TSource The type of elements of .
	 * @param source The  whose elements will be processed by .
	 * @param action An Action to invoke on each element.
	 */
	static function ForAll<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, action:cs.system.Action_1<TSource>):Void;
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.ParallelQuery_1<cs.system.linq.IGrouping<TKey, TSource>> {})
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<cs.system.linq.IGrouping<TKey, TSource>> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>):cs.system.linq.ParallelQuery_1<cs.system.linq.IGrouping<TKey, TElement>> {})
	@:overload(function<TSource, TKey, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TSource>, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<cs.system.linq.IGrouping<TKey, TElement>> {})
	@:overload(function<TSource, TKey, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TSource>, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TSource, TKey, TElement, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TElement>, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	/**
	 * Groups in parallel the elements of a sequence according to a specified key
	 * selector function.
	 * @param TSource The type of elements of .
	 * @param TKey The type of the key returned by .
	 * @param source An OrderedParallelQuery{TSource}that contains elements to sort.
	 * @param keySelector A function to extract a key from an element.
	 * @return A sequence of groups that are sorted descending according to .
	 */
	static function GroupBy<TSource, TKey, TElement, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TElement>, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.linq.ParallelQuery_1<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<TResult> {})
	/**
	 * This GroupJoin overload should never be called. This method is marked as
	 * obsolete and always throws  when called.
	 * @param TOuter This type parameter is not used.
	 * @param TInner This type parameter is not used.
	 * @param TKey This type parameter is not used.
	 * @param TResult This type parameter is not used.
	 * @param outer This parameter is not used.
	 * @param inner This parameter is not used.
	 * @param outerKeySelector This parameter is not used.
	 * @param innerKeySelector This parameter is not used.
	 * @param resultSelector This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function GroupJoin<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.linq.ParallelQuery_1<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * This Intersect overload should never be called. This method is marked as
	 * obsolete and always throws  when called.
	 * @param TSource This type parameter is not used.
	 * @param first This parameter is not used.
	 * @param second This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function Intersect<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, TInner, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.linq.ParallelQuery_1<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, TInner, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, TInner, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<TResult> {})
	/**
	 * This Join overload should never be called. This method is marked as obsolete and
	 * always throws  when invoked.
	 * @param TOuter This type parameter is not used.
	 * @param TInner This type parameter is not used.
	 * @param TKey This type parameter is not used.
	 * @param TResult This type parameter is not used.
	 * @param outer This parameter is not used.
	 * @param inner This parameter is not used.
	 * @param outerKeySelector This parameter is not used.
	 * @param innerKeySelector This parameter is not used.
	 * @param resultSelector This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function Join<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.ParallelQuery_1<TOuter>, inner:cs.system.linq.ParallelQuery_1<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, TInner, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	/**
	 * Returns the last element of a parallel sequence.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return the last element from.
	 * @return The value at the last position in the source sequence.
	 */
	static function Last<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	/**
	 * Returns the last element of a parallel sequence, or a default value if the
	 * sequence contains no elements.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return an element from.
	 * @return default() if the source sequence is empty; otherwise, the last element
	 * in the sequence.
	 */
	static function LastOrDefault<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):haxe.Int64 {})
	/**
	 * Returns an Int64 that represents the total number of elements in a parallel
	 * sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence that contains elements to be counted.
	 * @return The number of elements in the input sequence.
	 */
	static function LongCount<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):haxe.Int64;
	@:overload(function(source:cs.system.linq.ParallelQuery_1<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Float>):Float {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Int>):Int {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<haxe.Int64>):haxe.Int64 {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Int>>):Null<Int> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Int>):Int {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):haxe.Int64 {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Int> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Single>):Single {})
	/**
	 * Returns the maximum value in a parallel sequence of values.
	 * @param source A sequence of values to determine the maximum value of.
	 * @return The maximum value in the sequence.
	 */
	static function Max<TSource, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, TResult>):TResult;
	@:overload(function(source:cs.system.linq.ParallelQuery_1<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Float>):Float {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Int>):Int {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<haxe.Int64>):haxe.Int64 {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Int>>):Null<Int> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Int>):Int {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):haxe.Int64 {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Int> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Single>):Single {})
	/**
	 * Returns the minimum value in a parallel sequence of values.
	 * @param source A sequence of values to determine the minimum value of.
	 * @return The minimum value in the sequence.
	 */
	static function Min<TSource, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, TResult>):TResult;
	/**
	 * Filters the elements of a ParallelQuery based on a specified type.
	 * @param TResult The type to filter the elements of the sequence on.
	 * @param source The sequence with elements to filter.
	 * @return A sequence that contains elements from the input sequence filtered by
	 * type.
	 */
	static function OfType<TResult>(source:cs.system.linq.ParallelQuery):cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.OrderedParallelQuery<TSource> {})
	/**
	 * Sorts in parallel the elements of a sequence in ascending order according to a
	 * key.
	 * @param TSource The type of elements of .
	 * @param TKey The type of the key returned by .
	 * @param source A sequence of values to order.
	 * @param keySelector A function to extract a key from an element.
	 * @return An OrderedParallelQuery{TSource} whose elements are sorted according to
	 * a key.
	 */
	static function OrderBy<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.OrderedParallelQuery<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.OrderedParallelQuery<TSource> {})
	/**
	 * Sorts in parallel the elements of a sequence in descending order according to a
	 * key.
	 * @param TSource The type of elements of .
	 * @param TKey The type of the key returned by .
	 * @param source A sequence of values to order.
	 * @param keySelector A function to extract a key from an element.
	 * @return An OrderedParallelQuery{TSource} whose elements are sorted descending
	 * according to a key.
	 */
	static function OrderByDescending<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.OrderedParallelQuery<TSource>;
	/**
	 * Generates a parallel sequence of integral numbers within a specified range.
	 * @param start The value of the first integer in the sequence.
	 * @param count The number of sequential integers to generate.
	 * @return An IEnumerable<Int32> in C# or IEnumerable(Of Int32) in Visual Basic
	 * that contains a range of sequential integral numbers.
	 */
	static function Range(start:Int, count:Int):cs.system.linq.ParallelQuery_1<Int>;
	/**
	 * Generates a parallel sequence that contains one repeated value.
	 * @param TResult The type of the value to be repeated in the result sequence.
	 * @param element The value to be repeated.
	 * @param count The number of times to repeat the value in the generated sequence.
	 * @return A sequence that contains a repeated value.
	 */
	static function Repeat<TResult>(element:TResult, count:Int):cs.system.linq.ParallelQuery_1<TResult>;
	/**
	 * Inverts the order of the elements in a parallel sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence of values to reverse.
	 * @return A sequence whose elements correspond to those of the input sequence in
	 * reverse order.
	 */
	static function Reverse<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_3<TSource, Int, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	/**
	 * Projects in parallel each element of a sequence into a new form.
	 * @param TSource The type of the elements of .
	 * @param TResult The type of elements returned by selector.
	 * @param source A sequence of values to invoke a transform function on.
	 * @param selector A transform function to apply to each element.
	 * @return A sequence whose elements are the result of invoking the transform
	 * function on each element of .
	 */
	static function Select<TSource, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, TResult>):cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TSource, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, cs.system.collections.generic.IEnumerable<TResult>>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TSource, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_3<TSource, Int, cs.system.collections.generic.IEnumerable<TResult>>):cs.system.linq.ParallelQuery_1<TResult> {})
	@:overload(function<TSource, TCollection, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, collectionSelector:cs.system.Func_2<TSource, cs.system.collections.generic.IEnumerable<TCollection>>, resultSelector:cs.system.Func_3<TSource, TCollection, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	/**
	 * Projects in parallel each element of a sequence to an  and flattens the
	 * resulting sequences into one sequence.
	 * @param TSource The type of elements of .
	 * @param TResult The type of the elements of the sequence returned by selector.
	 * @param source A sequence of values to project.
	 * @param selector A transform function to apply to each element.
	 * @return A sequence whose elements are the result of invoking the one-to-many
	 * transform function on each element of the input sequence.
	 */
	static function SelectMany<TSource, TCollection, TResult>(source:cs.system.linq.ParallelQuery_1<TSource>, collectionSelector:cs.system.Func_3<TSource, Int, cs.system.collections.generic.IEnumerable<TCollection>>, resultSelector:cs.system.Func_3<TSource, TCollection, TResult>):cs.system.linq.ParallelQuery_1<TResult>;
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):Bool {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>):Bool {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):Bool {})
	/**
	 * This SequenceEqual overload should never be called. This method is marked as
	 * obsolete and always throws  when called.
	 * @param TSource This type parameter is not used.
	 * @param first This parameter is not used.
	 * @param second This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function SequenceEqual<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):Bool;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	/**
	 * Returns the only element of a parallel sequence, and throws an exception if
	 * there is not exactly one element in the sequence.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return the single element of.
	 * @return The single element of the input sequence.
	 */
	static function Single<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):TSource {})
	/**
	 * Returns the only element of a parallel sequence, or a default value if the
	 * sequence is empty; this method throws an exception if there is more than one
	 * element in the sequence.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return the single element of.
	 * @return The single element of the input sequence, or default() if the sequence
	 * contains no elements.
	 */
	static function SingleOrDefault<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	/**
	 * Bypasses a specified number of elements in a parallel sequence and then returns
	 * the remaining elements.
	 * @param TSource The type of elements of .
	 * @param source The sequence to return elements from.
	 * @param count The number of elements to skip before returning the remaining
	 * elements.
	 * @return A sequence that contains the elements that occur after the specified
	 * index in the input sequence.
	 */
	static function Skip<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, count:Int):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * Bypasses elements in a parallel sequence as long as a specified condition is
	 * true and then returns the remaining elements.
	 * @param TSource The type of elements of .
	 * @param source The sequence to return elements from.
	 * @param predicate A function to test each element for a condition.
	 * @return A sequence that contains the elements from the input sequence starting
	 * at the first element in the linear series that does not pass the test specified
	 * by predicate.
	 */
	static function SkipWhile<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_3<TSource, Int, Bool>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function(source:cs.system.linq.ParallelQuery_1<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Float>):Float {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Int>):Int {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<haxe.Int64>):haxe.Int64 {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Int>>):Null<Int> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.linq.ParallelQuery_1<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Int>):Int {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):haxe.Int64 {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Int> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	/**
	 * Computes in parallel the sum of a sequence of values.
	 * @param source A sequence of values to calculate the sum of.
	 * @return The sum of the projected values in the sequence.
	 */
	static function Sum<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, selector:cs.system.Func_2<TSource, Single>):Single;
	/**
	 * Returns a specified number of contiguous elements from the start of a parallel
	 * sequence.
	 * @param TSource The type of elements of .
	 * @param source The sequence to return elements from.
	 * @param count The number of elements to return.
	 * @return A sequence that contains the specified number of elements from the start
	 * of the input sequence.
	 */
	static function Take<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, count:Int):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * Returns elements from a parallel sequence as long as a specified condition is
	 * true.
	 * @param TSource The type of elements of .
	 * @param source The sequence to return elements from.
	 * @param predicate A function to test each element for a condition.
	 * @return A sequence that contains the elements from the input sequence that occur
	 * before the element at which the test no longer passes.
	 */
	static function TakeWhile<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_3<TSource, Int, Bool>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.OrderedParallelQuery<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.OrderedParallelQuery<TSource> {})
	/**
	 * Performs in parallel a subsequent ordering of the elements in a sequence in
	 * ascending order according to a key.
	 * @param TSource The type of elements of .
	 * @param TKey The type of the key returned by .
	 * @param source An OrderedParallelQuery{TSource} that contains elements to sort.
	 * @param keySelector A function to extract a key from an element.
	 * @return An OrderedParallelQuery{TSource} whose elements are sorted according to
	 * a key.
	 */
	static function ThenBy<TSource, TKey>(source:cs.system.linq.OrderedParallelQuery<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.OrderedParallelQuery<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.OrderedParallelQuery<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.OrderedParallelQuery<TSource> {})
	/**
	 * Performs in parallel a subsequent ordering of the elements in a sequence in
	 * descending order, according to a key.
	 * @param TSource The type of elements of .
	 * @param TKey The type of the key returned by .
	 * @param source An OrderedParallelQuery{TSource} that contains elements to sort.
	 * @param keySelector A function to extract a key from an element.
	 * @return A sequence whose elements are sorted descending according to a key.
	 */
	static function ThenByDescending<TSource, TKey>(source:cs.system.linq.OrderedParallelQuery<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.OrderedParallelQuery<TSource>;
	/**
	 * Creates an array from a .
	 * @param TSource The type of the elements of .
	 * @param source A sequence to create an array from.
	 * @return An array that contains the elements from the input sequence.
	 */
	static function ToArray<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.NativeArray<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.collections.generic.Dictionary<TKey, TSource> {})
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.Dictionary<TKey, TSource> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>):cs.system.collections.generic.Dictionary<TKey, TElement> {})
	/**
	 * Creates a  from a  according to a specified key selector function.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source A sequence to create a  from.
	 * @param keySelector A function to extract a key from each element.
	 * @return A  that contains keys and values.
	 */
	static function ToDictionary<TSource, TKey, TElement>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.Dictionary<TKey, TElement>;
	/**
	 * Creates a  from an .
	 * @param TSource The type of the elements of .
	 * @param source A sequence to create a  from.
	 * @return A  that contains elements from the input sequence.
	 */
	static function ToList<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>):cs.system.collections.generic.List<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.ILookup<TKey, TSource> {})
	@:overload(function<TSource, TKey>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ILookup<TKey, TSource> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>):cs.system.linq.ILookup<TKey, TElement> {})
	/**
	 * Creates an  from a  according to a specified key selector function.
	 * @param TSource The type of elements of .
	 * @param TKey The type of the key returned by .
	 * @param source The sequence to create a  from.
	 * @param keySelector A function to extract a key from each element.
	 * @return A  that contains keys and values.
	 */
	static function ToLookup<TSource, TKey, TElement>(source:cs.system.linq.ParallelQuery_1<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ILookup<TKey, TElement>;
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	@:overload(function<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * This Union overload should never be called. This method is marked as obsolete
	 * and always throws  when called.
	 * @param TSource This type parameter is not used.
	 * @param first This parameter is not used.
	 * @param second This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function Union<TSource>(first:cs.system.linq.ParallelQuery_1<TSource>, second:cs.system.linq.ParallelQuery_1<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_2<TSource, Bool>):cs.system.linq.ParallelQuery_1<TSource> {})
	/**
	 * Filters in parallel a sequence of values based on a predicate.
	 * @param TSource The type of the elements of source.
	 * @param source A sequence to filter.
	 * @param predicate A function to test each element for a condition.
	 * @return A sequence that contains elements from the input sequence that satisfy
	 * the condition.
	 */
	static function Where<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, predicate:cs.system.Func_3<TSource, Int, Bool>):cs.system.linq.ParallelQuery_1<TSource>;
	/**
	 * Sets the  to associate with the query.
	 * @param TSource The type of elements of .
	 * @param source A ParallelQuery on which to set the option.
	 * @param cancellationToken A cancellation token.
	 * @return ParallelQuery representing the same query as source, but with the
	 * registered cancellation token.
	 */
	static function WithCancellation<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, cancellationToken:cs.system.threading.CancellationToken):cs.system.linq.ParallelQuery_1<TSource>;
	/**
	 * Sets the degree of parallelism to use in a query. Degree of parallelism is the
	 * maximum number of concurrently executing tasks that will be used to process the
	 * query.
	 * @param TSource The type of elements of .
	 * @param source A ParallelQuery on which to set the limit on the degrees of
	 * parallelism.
	 * @param degreeOfParallelism The degree of parallelism for the query. The default
	 * value is Math.Min(, ) where  is 512.
	 * @return ParallelQuery representing the same query as source, with the limit on
	 * the degrees of parallelism set.
	 */
	static function WithDegreeOfParallelism<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, degreeOfParallelism:Int):cs.system.linq.ParallelQuery_1<TSource>;
	/**
	 * Sets the execution mode of the query.
	 * @param TSource The type of elements of .
	 * @param source A ParallelQuery on which to set the option.
	 * @param executionMode The mode in which to execute the query.
	 * @return ParallelQuery representing the same query as source, but with the
	 * registered execution mode.
	 */
	static function WithExecutionMode<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, executionMode:cs.system.linq.ParallelExecutionMode):cs.system.linq.ParallelQuery_1<TSource>;
	/**
	 * Sets the merge options for this query, which specify how the query will buffer
	 * output.
	 * @param TSource The type of elements of .
	 * @param source A ParallelQuery on which to set the option.
	 * @param mergeOptions The merge options to set for this query.
	 * @return ParallelQuery representing the same query as source, but with the
	 * registered merge options.
	 */
	static function WithMergeOptions<TSource>(source:cs.system.linq.ParallelQuery_1<TSource>, mergeOptions:cs.system.linq.ParallelMergeOptions):cs.system.linq.ParallelQuery_1<TSource>;
	@:overload(function<TFirst, TSecond, TResult>(first:cs.system.linq.ParallelQuery_1<TFirst>, second:cs.system.collections.generic.IEnumerable<TSecond>, resultSelector:cs.system.Func_3<TFirst, TSecond, TResult>):cs.system.linq.ParallelQuery_1<TResult> {})
	/**
	 * This Zip overload should never be called. This method is marked as obsolete and
	 * always throws  when invoked.
	 * @param TFirst This type parameter is not used.
	 * @param TSecond This type parameter is not used.
	 * @param TResult This type parameter is not used.
	 * @param first This parameter is not used.
	 * @param second This parameter is not used.
	 * @param resultSelector This parameter is not used.
	 * @return This overload always throws a .
	 */
	static function Zip<TFirst, TSecond, TResult>(first:cs.system.linq.ParallelQuery_1<TFirst>, second:cs.system.linq.ParallelQuery_1<TSecond>, resultSelector:cs.system.Func_3<TFirst, TSecond, TResult>):cs.system.linq.ParallelQuery_1<TResult>;
}
