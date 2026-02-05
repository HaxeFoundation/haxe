package cs.system.linq;

/** Provides a set of  ( in Visual Basic) methods for querying data structures that implement . */
@:native("System.Linq.Queryable")
extern class Queryable {
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, func:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, TSource, TSource>>):TSource {})
	@:overload(function<TSource, TAccumulate>(source:cs.system.linq.IQueryable_1<TSource>, seed:TAccumulate, func:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TAccumulate, TSource, TAccumulate>>):TAccumulate {})
	/**
	 * Applies an accumulator function over a sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence to aggregate over.
	 * @param func An accumulator function to apply to each element.
	 * @return The final accumulator value.
	 */
	static function Aggregate<TSource, TAccumulate, TResult>(source:cs.system.linq.IQueryable_1<TSource>, seed:TAccumulate, func:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TAccumulate, TSource, TAccumulate>>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TAccumulate, TResult>>):TResult;
	/**
	 * Determines whether all the elements of a sequence satisfy a condition.
	 * @param TSource The type of the elements of .
	 * @param source A sequence whose elements to test for a condition.
	 * @param predicate A function to test each element for a condition.
	 * @return if every element of the source sequence passes the test in the specified
	 * predicate, or if the sequence is empty; otherwise, .
	 */
	static function All<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):Bool;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):Bool {})
	/**
	 * Determines whether a sequence contains any elements.
	 * @param TSource The type of the elements of .
	 * @param source A sequence to check for being empty.
	 * @return if the source sequence contains any elements; otherwise, .
	 */
	static function Any<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):Bool;
	/**
	 * @param TSource 
	 * @param source 
	 * @param element 
	 */
	static function Append<TSource>(source:cs.system.linq.IQueryable_1<TSource>, element:TSource):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function(source:cs.system.collections.IEnumerable):cs.system.linq.IQueryable {})
	/**
	 * Converts an  to an .
	 * @param source A sequence to convert.
	 * @return An  that represents the input sequence.
	 */
	static function AsQueryable<TElement>(source:cs.system.collections.generic.IEnumerable<TElement>):cs.system.linq.IQueryable_1<TElement>;
	@:overload(function(source:cs.system.linq.IQueryable_1<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Float>):Float {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Int>):Float {})
	@:overload(function(source:cs.system.linq.IQueryable_1<haxe.Int64>):Float {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<Int>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<haxe.Int64>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, cs.system.Decimal>>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Float>>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Int>>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, haxe.Int64>>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<cs.system.Decimal>>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<Float>>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<Int>>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<haxe.Int64>>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<Single>>>):Null<Single> {})
	/**
	 * Computes the average of a sequence of  values.
	 * @param source A sequence of  values to calculate the average of.
	 * @return The average of the sequence of values.
	 */
	static function Average<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Single>>):Single;
	/**
	 * Converts the elements of an  to the specified type.
	 * @param TResult The type to convert the elements of  to.
	 * @param source The  that contains the elements to be converted.
	 * @return An  that contains each element of the source sequence converted to the
	 * specified type.
	 */
	static function Cast<TResult>(source:cs.system.linq.IQueryable):cs.system.linq.IQueryable_1<TResult>;
	/**
	 * Concatenates two sequences.
	 * @param TSource The type of the elements of the input sequences.
	 * @param source1 The first sequence to concatenate.
	 * @param source2 The sequence to concatenate to the first sequence.
	 * @return An  that contains the concatenated elements of the two input sequences.
	 */
	static function Concat<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, item:TSource):Bool {})
	/**
	 * Determines whether a sequence contains a specified element by using the default
	 * equality comparer.
	 * @param TSource The type of the elements of .
	 * @param source An  in which to locate .
	 * @param item The object to locate in the sequence.
	 * @return if the input sequence contains an element that has the specified value;
	 * otherwise, .
	 */
	static function Contains<TSource>(source:cs.system.linq.IQueryable_1<TSource>, item:TSource, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):Bool;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):Int {})
	/**
	 * Returns the number of elements in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source The  that contains the elements to be counted.
	 * @return The number of elements in the input sequence.
	 */
	static function Count<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):Int;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Returns the elements of the specified sequence or the type parameter's default
	 * value in a singleton collection if the sequence is empty.
	 * @param TSource The type of the elements of .
	 * @param source The  to return a default value for if empty.
	 * @return An  that contains () if  is empty; otherwise, .
	 */
	static function DefaultIfEmpty<TSource>(source:cs.system.linq.IQueryable_1<TSource>, defaultValue:TSource):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Returns distinct elements from a sequence by using the default equality comparer
	 * to compare values.
	 * @param TSource The type of the elements of .
	 * @param source The  to remove duplicates from.
	 * @return An  that contains distinct elements from .
	 */
	static function Distinct<TSource>(source:cs.system.linq.IQueryable_1<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.IQueryable_1<TSource>;
	/**
	 * Returns the element at a specified index in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return an element from.
	 * @param index The zero-based index of the element to retrieve.
	 * @return The element at the specified position in .
	 */
	static function ElementAt<TSource>(source:cs.system.linq.IQueryable_1<TSource>, index:Int):TSource;
	/**
	 * Returns the element at a specified index in a sequence or a default value if the
	 * index is out of range.
	 * @param TSource The type of the elements of .
	 * @param source An  to return an element from.
	 * @param index The zero-based index of the element to retrieve.
	 * @return default() if  is outside the bounds of ; otherwise, the element at the
	 * specified position in .
	 */
	static function ElementAtOrDefault<TSource>(source:cs.system.linq.IQueryable_1<TSource>, index:Int):TSource;
	@:overload(function<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Produces the set difference of two sequences by using the default equality
	 * comparer to compare values.
	 * @param TSource The type of the elements of the input sequences.
	 * @param source1 An  whose elements that are not also in  will be returned.
	 * @param source2 An  whose elements that also occur in the first sequence will not
	 * appear in the returned sequence.
	 * @return An  that contains the set difference of the two sequences.
	 */
	static function Except<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the first element of a sequence.
	 * @param TSource The type of the elements of .
	 * @param source The  to return the first element of.
	 * @return The first element in .
	 */
	static function First<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the first element of a sequence, or a default value if the sequence
	 * contains no elements.
	 * @param TSource The type of the elements of .
	 * @param source The  to return the first element of.
	 * @return default() if  is empty; otherwise, the first element in .
	 */
	static function FirstOrDefault<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):TSource;
	@:overload(function<TSource, TKey>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>):cs.system.linq.IQueryable_1<cs.system.linq.IGrouping<TKey, TSource>> {})
	@:overload(function<TSource, TKey>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.IQueryable_1<cs.system.linq.IGrouping<TKey, TSource>> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, elementSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TElement>>):cs.system.linq.IQueryable_1<cs.system.linq.IGrouping<TKey, TElement>> {})
	@:overload(function<TSource, TKey, TResult>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TSource>, TResult>>):cs.system.linq.IQueryable_1<TResult> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, elementSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TElement>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.IQueryable_1<cs.system.linq.IGrouping<TKey, TElement>> {})
	@:overload(function<TSource, TKey, TResult>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TSource>, TResult>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.IQueryable_1<TResult> {})
	@:overload(function<TSource, TKey, TElement, TResult>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, elementSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TElement>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TElement>, TResult>>):cs.system.linq.IQueryable_1<TResult> {})
	/**
	 * Groups the elements of a sequence according to a specified key selector
	 * function.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by the function represented in .
	 * @param source An  whose elements to group.
	 * @param keySelector A function to extract the key for each element.
	 * @return An IQueryable<IGrouping<TKey, TSource>> in C# or IQueryable(Of
	 * IGrouping(Of TKey, TSource)) in Visual Basic where each  object contains a
	 * sequence of objects and a key.
	 */
	static function GroupBy<TSource, TKey, TElement, TResult>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, elementSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TElement>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TElement>, TResult>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.IQueryable_1<TResult>;
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.IQueryable_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TOuter, TKey>>, innerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TInner, TKey>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>>):cs.system.linq.IQueryable_1<TResult> {})
	/**
	 * Correlates the elements of two sequences based on key equality and groups the
	 * results. The default equality comparer is used to compare keys.
	 * @param TOuter The type of the elements of the first sequence.
	 * @param TInner The type of the elements of the second sequence.
	 * @param TKey The type of the keys returned by the key selector functions.
	 * @param TResult The type of the result elements.
	 * @param outer The first sequence to join.
	 * @param inner The sequence to join to the first sequence.
	 * @param outerKeySelector A function to extract the join key from each element of
	 * the first sequence.
	 * @param innerKeySelector A function to extract the join key from each element of
	 * the second sequence.
	 * @param resultSelector A function to create a result element from an element from
	 * the first sequence and a collection of matching elements from the second
	 * sequence.
	 * @return An  that contains elements of type  obtained by performing a grouped
	 * join on two sequences.
	 */
	static function GroupJoin<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.IQueryable_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TOuter, TKey>>, innerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TInner, TKey>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.IQueryable_1<TResult>;
	@:overload(function<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Produces the set intersection of two sequences by using the default equality
	 * comparer to compare values.
	 * @param TSource The type of the elements of the input sequences.
	 * @param source1 A sequence whose distinct elements that also appear in  are
	 * returned.
	 * @param source2 A sequence whose distinct elements that also appear in the first
	 * sequence are returned.
	 * @return A sequence that contains the set intersection of the two sequences.
	 */
	static function Intersect<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.IQueryable_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TOuter, TKey>>, innerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TInner, TKey>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TOuter, TInner, TResult>>):cs.system.linq.IQueryable_1<TResult> {})
	/**
	 * Correlates the elements of two sequences based on matching keys. The default
	 * equality comparer is used to compare keys.
	 * @param TOuter The type of the elements of the first sequence.
	 * @param TInner The type of the elements of the second sequence.
	 * @param TKey The type of the keys returned by the key selector functions.
	 * @param TResult The type of the result elements.
	 * @param outer The first sequence to join.
	 * @param inner The sequence to join to the first sequence.
	 * @param outerKeySelector A function to extract the join key from each element of
	 * the first sequence.
	 * @param innerKeySelector A function to extract the join key from each element of
	 * the second sequence.
	 * @param resultSelector A function to create a result element from two matching
	 * elements.
	 * @return An  that has elements of type  obtained by performing an inner join on
	 * two sequences.
	 */
	static function Join<TOuter, TInner, TKey, TResult>(outer:cs.system.linq.IQueryable_1<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TOuter, TKey>>, innerKeySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TInner, TKey>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TOuter, TInner, TResult>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.IQueryable_1<TResult>;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the last element in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the last element of.
	 * @return The value at the last position in .
	 */
	static function Last<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the last element in a sequence, or a default value if the sequence
	 * contains no elements.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the last element of.
	 * @return default() if  is empty; otherwise, the last element in .
	 */
	static function LastOrDefault<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):haxe.Int64 {})
	/**
	 * Returns an  that represents the total number of elements in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  that contains the elements to be counted.
	 * @return The number of elements in .
	 */
	static function LongCount<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):haxe.Int64;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the maximum value in a generic .
	 * @param TSource The type of the elements of .
	 * @param source A sequence of values to determine the maximum of.
	 * @return The maximum value in the sequence.
	 */
	static function Max<TSource, TResult>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TResult>>):TResult;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the minimum value of a generic .
	 * @param TSource The type of the elements of .
	 * @param source A sequence of values to determine the minimum of.
	 * @return The minimum value in the sequence.
	 */
	static function Min<TSource, TResult>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TResult>>):TResult;
	/**
	 * Filters the elements of an  based on a specified type.
	 * @param TResult The type to filter the elements of the sequence on.
	 * @param source An  whose elements to filter.
	 * @return A collection that contains the elements from  that have type .
	 */
	static function OfType<TResult>(source:cs.system.linq.IQueryable):cs.system.linq.IQueryable_1<TResult>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>):cs.system.linq.IOrderedQueryable_1<TSource> {})
	/**
	 * Sorts the elements of a sequence in ascending order according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by the function that is represented by
	 * .
	 * @param source A sequence of values to order.
	 * @param keySelector A function to extract a key from an element.
	 * @return An  whose elements are sorted according to a key.
	 */
	static function OrderBy<TSource, TKey>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedQueryable_1<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>):cs.system.linq.IOrderedQueryable_1<TSource> {})
	/**
	 * Sorts the elements of a sequence in descending order according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by the function that is represented by
	 * .
	 * @param source A sequence of values to order.
	 * @param keySelector A function to extract a key from an element.
	 * @return An  whose elements are sorted in descending order according to a key.
	 */
	static function OrderByDescending<TSource, TKey>(source:cs.system.linq.IQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedQueryable_1<TSource>;
	/**
	 * @param TSource 
	 * @param source 
	 * @param element 
	 */
	static function Prepend<TSource>(source:cs.system.linq.IQueryable_1<TSource>, element:TSource):cs.system.linq.IQueryable_1<TSource>;
	/**
	 * Inverts the order of the elements in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence of values to reverse.
	 * @return An  whose elements correspond to those of the input sequence in reverse
	 * order.
	 */
	static function Reverse<TSource>(source:cs.system.linq.IQueryable_1<TSource>):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource, TResult>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, Int, TResult>>):cs.system.linq.IQueryable_1<TResult> {})
	/**
	 * Projects each element of a sequence into a new form.
	 * @param TSource The type of the elements of .
	 * @param TResult The type of the value returned by the function represented by .
	 * @param source A sequence of values to project.
	 * @param selector A projection function to apply to each element.
	 * @return An  whose elements are the result of invoking a projection function on
	 * each element of .
	 */
	static function Select<TSource, TResult>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TResult>>):cs.system.linq.IQueryable_1<TResult>;
	@:overload(function<TSource, TResult>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, cs.system.collections.generic.IEnumerable<TResult>>>):cs.system.linq.IQueryable_1<TResult> {})
	@:overload(function<TSource, TResult>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, Int, cs.system.collections.generic.IEnumerable<TResult>>>):cs.system.linq.IQueryable_1<TResult> {})
	@:overload(function<TSource, TCollection, TResult>(source:cs.system.linq.IQueryable_1<TSource>, collectionSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, cs.system.collections.generic.IEnumerable<TCollection>>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, TCollection, TResult>>):cs.system.linq.IQueryable_1<TResult> {})
	/**
	 * Projects each element of a sequence to an  and combines the resulting sequences
	 * into one sequence.
	 * @param TSource The type of the elements of .
	 * @param TResult The type of the elements of the sequence returned by the function
	 * represented by .
	 * @param source A sequence of values to project.
	 * @param selector A projection function to apply to each element.
	 * @return An  whose elements are the result of invoking a one-to-many projection
	 * function on each element of the input sequence.
	 */
	static function SelectMany<TSource, TCollection, TResult>(source:cs.system.linq.IQueryable_1<TSource>, collectionSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, Int, cs.system.collections.generic.IEnumerable<TCollection>>>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, TCollection, TResult>>):cs.system.linq.IQueryable_1<TResult>;
	@:overload(function<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>):Bool {})
	/**
	 * Determines whether two sequences are equal by using the default equality
	 * comparer to compare elements.
	 * @param TSource The type of the elements of the input sequences.
	 * @param source1 An  whose elements to compare to those of .
	 * @param source2 An  whose elements to compare to those of the first sequence.
	 * @return if the two source sequences are of equal length and their corresponding
	 * elements compare equal; otherwise, .
	 */
	static function SequenceEqual<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):Bool;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the only element of a sequence, and throws an exception if there is not
	 * exactly one element in the sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the single element of.
	 * @return The single element of the input sequence.
	 */
	static function Single<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):TSource;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>):TSource {})
	/**
	 * Returns the only element of a sequence, or a default value if the sequence is
	 * empty; this method throws an exception if there is more than one element in the
	 * sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the single element of.
	 * @return The single element of the input sequence, or default() if the sequence
	 * contains no elements.
	 */
	static function SingleOrDefault<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):TSource;
	/**
	 * Bypasses a specified number of elements in a sequence and then returns the
	 * remaining elements.
	 * @param TSource The type of the elements of .
	 * @param source An  to return elements from.
	 * @param count The number of elements to skip before returning the remaining
	 * elements.
	 * @return An  that contains elements that occur after the specified index in the
	 * input sequence.
	 */
	static function Skip<TSource>(source:cs.system.linq.IQueryable_1<TSource>, count:Int):cs.system.linq.IQueryable_1<TSource>;
	/**
	 * @param TSource 
	 * @param source 
	 * @param count 
	 */
	static function SkipLast<TSource>(source:cs.system.linq.IQueryable_1<TSource>, count:Int):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Bypasses elements in a sequence as long as a specified condition is true and
	 * then returns the remaining elements.
	 * @param TSource The type of the elements of .
	 * @param source An  to return elements from.
	 * @param predicate A function to test each element for a condition.
	 * @return An  that contains elements from  starting at the first element in the
	 * linear series that does not pass the test specified by .
	 */
	static function SkipWhile<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, Int, Bool>>):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function(source:cs.system.linq.IQueryable_1<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Float>):Float {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Int>):Int {})
	@:overload(function(source:cs.system.linq.IQueryable_1<haxe.Int64>):haxe.Int64 {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<Int>>):Null<Int> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.linq.IQueryable_1<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, cs.system.Decimal>>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Float>>):Float {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Int>>):Int {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, haxe.Int64>>):haxe.Int64 {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<cs.system.Decimal>>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<Float>>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<Int>>>):Null<Int> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<haxe.Int64>>>):Null<haxe.Int64> {})
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Null<Single>>>):Null<Single> {})
	/**
	 * Computes the sum of a sequence of  values.
	 * @param source A sequence of  values to calculate the sum of.
	 * @return The sum of the values in the sequence.
	 */
	static function Sum<TSource>(source:cs.system.linq.IQueryable_1<TSource>, selector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Single>>):Single;
	/**
	 * Returns a specified number of contiguous elements from the start of a sequence.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return elements from.
	 * @param count The number of elements to return.
	 * @return An  that contains the specified number of elements from the start of .
	 */
	static function Take<TSource>(source:cs.system.linq.IQueryable_1<TSource>, count:Int):cs.system.linq.IQueryable_1<TSource>;
	/**
	 * @param TSource 
	 * @param source 
	 * @param count 
	 */
	static function TakeLast<TSource>(source:cs.system.linq.IQueryable_1<TSource>, count:Int):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Returns elements from a sequence as long as a specified condition is true.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return elements from.
	 * @param predicate A function to test each element for a condition.
	 * @return An  that contains elements from the input sequence occurring before the
	 * element at which the test specified by  no longer passes.
	 */
	static function TakeWhile<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, Int, Bool>>):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.IOrderedQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>):cs.system.linq.IOrderedQueryable_1<TSource> {})
	/**
	 * Performs a subsequent ordering of the elements in a sequence in ascending order
	 * according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by the function represented by .
	 * @param source An  that contains elements to sort.
	 * @param keySelector A function to extract a key from each element.
	 * @return An  whose elements are sorted according to a key.
	 */
	static function ThenBy<TSource, TKey>(source:cs.system.linq.IOrderedQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedQueryable_1<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.IOrderedQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>):cs.system.linq.IOrderedQueryable_1<TSource> {})
	/**
	 * Performs a subsequent ordering of the elements in a sequence in descending
	 * order, according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by the function represented by .
	 * @param source An  that contains elements to sort.
	 * @param keySelector A function to extract a key from each element.
	 * @return An  whose elements are sorted in descending order according to a key.
	 */
	static function ThenByDescending<TSource, TKey>(source:cs.system.linq.IOrderedQueryable_1<TSource>, keySelector:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, TKey>>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedQueryable_1<TSource>;
	@:overload(function<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Produces the set union of two sequences by using the default equality comparer.
	 * @param TSource The type of the elements of the input sequences.
	 * @param source1 A sequence whose distinct elements form the first set for the
	 * union operation.
	 * @param source2 A sequence whose distinct elements form the second set for the
	 * union operation.
	 * @return An  that contains the elements from both input sequences, excluding
	 * duplicates.
	 */
	static function Union<TSource>(source1:cs.system.linq.IQueryable_1<TSource>, source2:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.linq.IQueryable_1<TSource>;
	@:overload(function<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_2<TSource, Bool>>):cs.system.linq.IQueryable_1<TSource> {})
	/**
	 * Filters a sequence of values based on a predicate.
	 * @param TSource The type of the elements of .
	 * @param source An  to filter.
	 * @param predicate A function to test each element for a condition.
	 * @return An  that contains elements from the input sequence that satisfy the
	 * condition specified by .
	 */
	static function Where<TSource>(source:cs.system.linq.IQueryable_1<TSource>, predicate:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TSource, Int, Bool>>):cs.system.linq.IQueryable_1<TSource>;
	/**
	 * Merges two sequences by using the specified predicate function.
	 * @param TFirst The type of the elements of the first input sequence.
	 * @param TSecond The type of the elements of the second input sequence.
	 * @param TResult The type of the elements of the result sequence.
	 * @param source1 The first sequence to merge.
	 * @param source2 The second sequence to merge.
	 * @param resultSelector A function that specifies how to merge the elements from
	 * the two sequences.
	 * @return An  that contains merged elements of two input sequences.
	 */
	static function Zip<TFirst, TSecond, TResult>(source1:cs.system.linq.IQueryable_1<TFirst>, source2:cs.system.collections.generic.IEnumerable<TSecond>, resultSelector:cs.system.linq.expressions.Expression_1<cs.system.Func_3<TFirst, TSecond, TResult>>):cs.system.linq.IQueryable_1<TResult>;
}
