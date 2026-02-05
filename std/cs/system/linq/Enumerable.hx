package cs.system.linq;

/** Provides a set of  ( in Visual Basic) methods for querying objects that implement . */
@:native("System.Linq.Enumerable")
extern class Enumerable {
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, func:cs.system.Func_3<TSource, TSource, TSource>):TSource {})
	@:overload(function<TSource, TAccumulate>(source:cs.system.collections.generic.IEnumerable<TSource>, seed:TAccumulate, func:cs.system.Func_3<TAccumulate, TSource, TAccumulate>):TAccumulate {})
	/**
	 * Applies an accumulator function over a sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to aggregate over.
	 * @param func An accumulator function to be invoked on each element.
	 * @return The final accumulator value.
	 */
	static function Aggregate<TSource, TAccumulate, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, seed:TAccumulate, func:cs.system.Func_3<TAccumulate, TSource, TAccumulate>, resultSelector:cs.system.Func_2<TAccumulate, TResult>):TResult;
	/**
	 * Determines whether all elements of a sequence satisfy a condition.
	 * @param TSource The type of the elements of .
	 * @param source An  that contains the elements to apply the predicate to.
	 * @param predicate A function to test each element for a condition.
	 * @return if every element of the source sequence passes the test in the specified
	 * predicate, or if the sequence is empty; otherwise, .
	 */
	static function All<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):Bool;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):Bool {})
	/**
	 * Determines whether a sequence contains any elements.
	 * @param TSource The type of the elements of .
	 * @param source The  to check for emptiness.
	 * @return if the source sequence contains any elements; otherwise, .
	 */
	static function Any<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):Bool;
	/**
	 * Appends a value to the end of the sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence of values.
	 * @param element The value to append to .
	 * @return A new sequence that ends with .
	 */
	static function Append<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, element:TSource):cs.system.collections.generic.IEnumerable<TSource>;
	/**
	 * Returns the input typed as .
	 * @param TSource The type of the elements of .
	 * @param source The sequence to type as .
	 * @return The input sequence typed as .
	 */
	static function AsEnumerable<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Float>):Float {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Int>):Float {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<haxe.Int64>):Float {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Int>>):Null<Float> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<haxe.Int64>>):Null<Float> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Int>):Float {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):Float {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	/**
	 * Computes the average of a sequence of  values.
	 * @param source A sequence of  values to calculate the average of.
	 * @return The average of the sequence of values.
	 */
	static function Average<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Single>):Single;
	/**
	 * Casts the elements of an  to the specified type.
	 * @param TResult The type to cast the elements of  to.
	 * @param source The  that contains the elements to be cast to type .
	 * @return An  that contains each element of the source sequence cast to the
	 * specified type.
	 */
	static function Cast<TResult>(source:cs.system.collections.IEnumerable):cs.system.collections.generic.IEnumerable<TResult>;
	/**
	 * Concatenates two sequences.
	 * @param TSource The type of the elements of the input sequences.
	 * @param first The first sequence to concatenate.
	 * @param second The sequence to concatenate to the first sequence.
	 * @return An  that contains the concatenated elements of the two input sequences.
	 */
	static function Concat<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, value:TSource):Bool {})
	/**
	 * Determines whether a sequence contains a specified element by using the default
	 * equality comparer.
	 * @param TSource The type of the elements of .
	 * @param source A sequence in which to locate a value.
	 * @param value The value to locate in the sequence.
	 * @return if the source sequence contains an element that has the specified value;
	 * otherwise, .
	 */
	static function Contains<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, value:TSource, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):Bool;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):Int {})
	/**
	 * Returns the number of elements in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence that contains elements to be counted.
	 * @return The number of elements in the input sequence.
	 */
	static function Count<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):Int;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Returns the elements of the specified sequence or the type parameter's default
	 * value in a singleton collection if the sequence is empty.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return a default value for if it is empty.
	 * @return An  object that contains the default value for the  type if  is empty;
	 * otherwise, .
	 */
	static function DefaultIfEmpty<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, defaultValue:TSource):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Returns distinct elements from a sequence by using the default equality comparer
	 * to compare values.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to remove duplicate elements from.
	 * @return An  that contains distinct elements from the source sequence.
	 */
	static function Distinct<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	/**
	 * Returns the element at a specified index in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return an element from.
	 * @param index The zero-based index of the element to retrieve.
	 * @return The element at the specified position in the source sequence.
	 */
	static function ElementAt<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, index:Int):TSource;
	/**
	 * Returns the element at a specified index in a sequence or a default value if the
	 * index is out of range.
	 * @param TSource The type of the elements of .
	 * @param source An  to return an element from.
	 * @param index The zero-based index of the element to retrieve.
	 * @return () if the index is outside the bounds of the source sequence; otherwise,
	 * the element at the specified position in the source sequence.
	 */
	static function ElementAtOrDefault<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, index:Int):TSource;
	/**
	 * Returns an empty  that has the specified type argument.
	 * @param TResult The type to assign to the type parameter of the returned generic
	 * .
	 * @return An empty  whose type argument is .
	 */
	static function Empty<TResult>():cs.system.collections.generic.IEnumerable<TResult>;
	@:overload(function<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Produces the set difference of two sequences by using the default equality
	 * comparer to compare values.
	 * @param TSource The type of the elements of the input sequences.
	 * @param first An  whose elements that are not also in  will be returned.
	 * @param second An  whose elements that also occur in the first sequence will
	 * cause those elements to be removed from the returned sequence.
	 * @return A sequence that contains the set difference of the elements of two
	 * sequences.
	 */
	static function Except<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	/**
	 * Returns the first element of a sequence.
	 * @param TSource The type of the elements of .
	 * @param source The  to return the first element of.
	 * @return The first element in the specified sequence.
	 */
	static function First<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	/**
	 * Returns the first element of a sequence, or a default value if the sequence
	 * contains no elements.
	 * @param TSource The type of the elements of .
	 * @param source The  to return the first element of.
	 * @return () if  is empty; otherwise, the first element in .
	 */
	static function FirstOrDefault<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.collections.generic.IEnumerable<cs.system.linq.IGrouping<TKey, TSource>> {})
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.IEnumerable<cs.system.linq.IGrouping<TKey, TSource>> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>):cs.system.collections.generic.IEnumerable<cs.system.linq.IGrouping<TKey, TElement>> {})
	@:overload(function<TSource, TKey, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TSource>, TResult>):cs.system.collections.generic.IEnumerable<TResult> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.IEnumerable<cs.system.linq.IGrouping<TKey, TElement>> {})
	@:overload(function<TSource, TKey, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TSource>, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.IEnumerable<TResult> {})
	@:overload(function<TSource, TKey, TElement, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TElement>, TResult>):cs.system.collections.generic.IEnumerable<TResult> {})
	/**
	 * Groups the elements of a sequence according to a specified key selector
	 * function.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source An  whose elements to group.
	 * @param keySelector A function to extract the key for each element.
	 * @return An IEnumerable<IGrouping<TKey, TSource>> in C# or IEnumerable(Of
	 * IGrouping(Of TKey, TSource)) in Visual Basic where each  object contains a
	 * sequence of objects and a key.
	 */
	static function GroupBy<TSource, TKey, TElement, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, resultSelector:cs.system.Func_3<TKey, cs.system.collections.generic.IEnumerable<TElement>, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.IEnumerable<TResult>;
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.collections.generic.IEnumerable<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>):cs.system.collections.generic.IEnumerable<TResult> {})
	/**
	 * Correlates the elements of two sequences based on equality of keys and groups
	 * the results. The default equality comparer is used to compare keys.
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
	 * @return An  that contains elements of type  that are obtained by performing a
	 * grouped join on two sequences.
	 */
	static function GroupJoin<TOuter, TInner, TKey, TResult>(outer:cs.system.collections.generic.IEnumerable<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, cs.system.collections.generic.IEnumerable<TInner>, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.IEnumerable<TResult>;
	@:overload(function<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Produces the set intersection of two sequences by using the default equality
	 * comparer to compare values.
	 * @param TSource The type of the elements of the input sequences.
	 * @param first An  whose distinct elements that also appear in  will be returned.
	 * @param second An  whose distinct elements that also appear in the first sequence
	 * will be returned.
	 * @return A sequence that contains the elements that form the set intersection of
	 * two sequences.
	 */
	static function Intersect<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TOuter, TInner, TKey, TResult>(outer:cs.system.collections.generic.IEnumerable<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, TInner, TResult>):cs.system.collections.generic.IEnumerable<TResult> {})
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
	 * @return An  that has elements of type  that are obtained by performing an inner
	 * join on two sequences.
	 */
	static function Join<TOuter, TInner, TKey, TResult>(outer:cs.system.collections.generic.IEnumerable<TOuter>, inner:cs.system.collections.generic.IEnumerable<TInner>, outerKeySelector:cs.system.Func_2<TOuter, TKey>, innerKeySelector:cs.system.Func_2<TInner, TKey>, resultSelector:cs.system.Func_3<TOuter, TInner, TResult>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.IEnumerable<TResult>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	/**
	 * Returns the last element of a sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the last element of.
	 * @return The value at the last position in the source sequence.
	 */
	static function Last<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	/**
	 * Returns the last element of a sequence, or a default value if the sequence
	 * contains no elements.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the last element of.
	 * @return () if the source sequence is empty; otherwise, the last element in the .
	 */
	static function LastOrDefault<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):haxe.Int64 {})
	/**
	 * Returns an  that represents the total number of elements in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  that contains the elements to be counted.
	 * @return The number of elements in the source sequence.
	 */
	static function LongCount<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):haxe.Int64;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Float>):Float {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Int>):Int {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<haxe.Int64>):haxe.Int64 {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Int>>):Null<Int> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Int>):Int {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):haxe.Int64 {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Int> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Single>):Single {})
	/**
	 * Returns the maximum value in a sequence of  values.
	 * @param source A sequence of  values to determine the maximum value of.
	 * @return The maximum value in the sequence.
	 */
	static function Max<TSource, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, TResult>):TResult;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Float>):Float {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Int>):Int {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<haxe.Int64>):haxe.Int64 {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Int>>):Null<Int> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Int>):Int {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):haxe.Int64 {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Int> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Single>):Single {})
	/**
	 * Returns the minimum value in a sequence of  values.
	 * @param source A sequence of  values to determine the minimum value of.
	 * @return The minimum value in the sequence.
	 */
	static function Min<TSource, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, TResult>):TResult;
	/**
	 * Filters the elements of an  based on a specified type.
	 * @param TResult The type to filter the elements of the sequence on.
	 * @param source The  whose elements to filter.
	 * @return An  that contains elements from the input sequence of type .
	 */
	static function OfType<TResult>(source:cs.system.collections.IEnumerable):cs.system.collections.generic.IEnumerable<TResult>;
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.IOrderedEnumerable<TSource> {})
	/**
	 * Sorts the elements of a sequence in ascending order according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source A sequence of values to order.
	 * @param keySelector A function to extract a key from an element.
	 * @return An  whose elements are sorted according to a key.
	 */
	static function OrderBy<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedEnumerable<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.IOrderedEnumerable<TSource> {})
	/**
	 * Sorts the elements of a sequence in descending order according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source A sequence of values to order.
	 * @param keySelector A function to extract a key from an element.
	 * @return An  whose elements are sorted in descending order according to a key.
	 */
	static function OrderByDescending<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedEnumerable<TSource>;
	/**
	 * Adds a value to the beginning of the sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence of values.
	 * @param element The value to prepend to .
	 * @return A new sequence that begins with .
	 */
	static function Prepend<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, element:TSource):cs.system.collections.generic.IEnumerable<TSource>;
	/**
	 * Generates a sequence of integral numbers within a specified range.
	 * @param start The value of the first integer in the sequence.
	 * @param count The number of sequential integers to generate.
	 * @return An IEnumerable<Int32> in C# or IEnumerable(Of Int32) in Visual Basic
	 * that contains a range of sequential integral numbers.
	 */
	static function Range(start:Int, count:Int):cs.system.collections.generic.IEnumerable<Int>;
	/**
	 * Generates a sequence that contains one repeated value.
	 * @param TResult The type of the value to be repeated in the result sequence.
	 * @param element The value to be repeated.
	 * @param count The number of times to repeat the value in the generated sequence.
	 * @return An  that contains a repeated value.
	 */
	static function Repeat<TResult>(element:TResult, count:Int):cs.system.collections.generic.IEnumerable<TResult>;
	/**
	 * Inverts the order of the elements in a sequence.
	 * @param TSource The type of the elements of .
	 * @param source A sequence of values to reverse.
	 * @return A sequence whose elements correspond to those of the input sequence in
	 * reverse order.
	 */
	static function Reverse<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_3<TSource, Int, TResult>):cs.system.collections.generic.IEnumerable<TResult> {})
	/**
	 * Projects each element of a sequence into a new form.
	 * @param TSource The type of the elements of .
	 * @param TResult The type of the value returned by .
	 * @param source A sequence of values to invoke a transform function on.
	 * @param selector A transform function to apply to each element.
	 * @return An  whose elements are the result of invoking the transform function on
	 * each element of .
	 */
	static function Select<TSource, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, TResult>):cs.system.collections.generic.IEnumerable<TResult>;
	@:overload(function<TSource, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, cs.system.collections.generic.IEnumerable<TResult>>):cs.system.collections.generic.IEnumerable<TResult> {})
	@:overload(function<TSource, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_3<TSource, Int, cs.system.collections.generic.IEnumerable<TResult>>):cs.system.collections.generic.IEnumerable<TResult> {})
	@:overload(function<TSource, TCollection, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, collectionSelector:cs.system.Func_2<TSource, cs.system.collections.generic.IEnumerable<TCollection>>, resultSelector:cs.system.Func_3<TSource, TCollection, TResult>):cs.system.collections.generic.IEnumerable<TResult> {})
	/**
	 * Projects each element of a sequence to an  and flattens the resulting sequences
	 * into one sequence.
	 * @param TSource The type of the elements of .
	 * @param TResult The type of the elements of the sequence returned by .
	 * @param source A sequence of values to project.
	 * @param selector A transform function to apply to each element.
	 * @return An  whose elements are the result of invoking the one-to-many transform
	 * function on each element of the input sequence.
	 */
	static function SelectMany<TSource, TCollection, TResult>(source:cs.system.collections.generic.IEnumerable<TSource>, collectionSelector:cs.system.Func_3<TSource, Int, cs.system.collections.generic.IEnumerable<TCollection>>, resultSelector:cs.system.Func_3<TSource, TCollection, TResult>):cs.system.collections.generic.IEnumerable<TResult>;
	@:overload(function<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):Bool {})
	/**
	 * Determines whether two sequences are equal by comparing the elements by using
	 * the default equality comparer for their type.
	 * @param TSource The type of the elements of the input sequences.
	 * @param first An  to compare to .
	 * @param second An  to compare to the first sequence.
	 * @return if the two source sequences are of equal length and their corresponding
	 * elements are equal according to the default equality comparer for their type;
	 * otherwise, .
	 */
	static function SequenceEqual<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):Bool;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	/**
	 * Returns the only element of a sequence, and throws an exception if there is not
	 * exactly one element in the sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the single element of.
	 * @return The single element of the input sequence.
	 */
	static function Single<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):TSource {})
	/**
	 * Returns the only element of a sequence, or a default value if the sequence is
	 * empty; this method throws an exception if there is more than one element in the
	 * sequence.
	 * @param TSource The type of the elements of .
	 * @param source An  to return the single element of.
	 * @return The single element of the input sequence, or () if the sequence contains
	 * no elements.
	 */
	static function SingleOrDefault<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):TSource;
	/**
	 * Bypasses a specified number of elements in a sequence and then returns the
	 * remaining elements.
	 * @param TSource The type of the elements of .
	 * @param source An  to return elements from.
	 * @param count The number of elements to skip before returning the remaining
	 * elements.
	 * @return An  that contains the elements that occur after the specified index in
	 * the input sequence.
	 */
	static function Skip<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, count:Int):cs.system.collections.generic.IEnumerable<TSource>;
	/**
	 * @param TSource 
	 * @param source 
	 * @param count 
	 */
	static function SkipLast<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, count:Int):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Bypasses elements in a sequence as long as a specified condition is true and
	 * then returns the remaining elements.
	 * @param TSource The type of the elements of .
	 * @param source An  to return elements from.
	 * @param predicate A function to test each element for a condition.
	 * @return An  that contains the elements from the input sequence starting at the
	 * first element in the linear series that does not pass the test specified by .
	 */
	static function SkipWhile<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_3<TSource, Int, Bool>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function(source:cs.system.collections.generic.IEnumerable<cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Float>):Float {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Int>):Int {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<haxe.Int64>):haxe.Int64 {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Float>>):Null<Float> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Int>>):Null<Int> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Null<Single>>):Null<Single> {})
	@:overload(function(source:cs.system.collections.generic.IEnumerable<Single>):Single {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, cs.system.Decimal>):cs.system.Decimal {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Float>):Float {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Int>):Int {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, haxe.Int64>):haxe.Int64 {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<cs.system.Decimal>>):Null<cs.system.Decimal> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Float>>):Null<Float> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Int>>):Null<Int> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<haxe.Int64>>):Null<haxe.Int64> {})
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Null<Single>>):Null<Single> {})
	/**
	 * Computes the sum of a sequence of  values.
	 * @param source A sequence of  values to calculate the sum of.
	 * @return The sum of the values in the sequence.
	 */
	static function Sum<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, selector:cs.system.Func_2<TSource, Single>):Single;
	/**
	 * Returns a specified number of contiguous elements from the start of a sequence.
	 * @param TSource The type of the elements of .
	 * @param source The sequence to return elements from.
	 * @param count The number of elements to return.
	 * @return An  that contains the specified number of elements from the start of the
	 * input sequence.
	 */
	static function Take<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, count:Int):cs.system.collections.generic.IEnumerable<TSource>;
	/**
	 * @param TSource 
	 * @param source 
	 * @param count 
	 */
	static function TakeLast<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, count:Int):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Returns elements from a sequence as long as a specified condition is true.
	 * @param TSource The type of the elements of .
	 * @param source A sequence to return elements from.
	 * @param predicate A function to test each element for a condition.
	 * @return An  that contains the elements from the input sequence that occur before
	 * the element at which the test no longer passes.
	 */
	static function TakeWhile<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_3<TSource, Int, Bool>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.IOrderedEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.IOrderedEnumerable<TSource> {})
	/**
	 * Performs a subsequent ordering of the elements in a sequence in ascending order
	 * according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source An  that contains elements to sort.
	 * @param keySelector A function to extract a key from each element.
	 * @return An  whose elements are sorted according to a key.
	 */
	static function ThenBy<TSource, TKey>(source:cs.system.linq.IOrderedEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedEnumerable<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.linq.IOrderedEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.IOrderedEnumerable<TSource> {})
	/**
	 * Performs a subsequent ordering of the elements in a sequence in descending
	 * order, according to a key.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source An  that contains elements to sort.
	 * @param keySelector A function to extract a key from each element.
	 * @return An  whose elements are sorted in descending order according to a key.
	 */
	static function ThenByDescending<TSource, TKey>(source:cs.system.linq.IOrderedEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.linq.IOrderedEnumerable<TSource>;
	/**
	 * Creates an array from a .
	 * @param TSource The type of the elements of .
	 * @param source An  to create an array from.
	 * @return An array that contains the elements from the input sequence.
	 */
	static function ToArray<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.NativeArray<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.collections.generic.Dictionary<TKey, TSource> {})
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.Dictionary<TKey, TSource> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>):cs.system.collections.generic.Dictionary<TKey, TElement> {})
	/**
	 * Creates a  from an  according to a specified key selector function.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source An  to create a  from.
	 * @param keySelector A function to extract a key from each element.
	 * @return A  that contains keys and values.
	 */
	static function ToDictionary<TSource, TKey, TElement>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.collections.generic.Dictionary<TKey, TElement>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.HashSet<TSource> {})
	/**
	 * Creates a  from an .
	 * @param TSource The type of the elements of .
	 * @param source An  to create a  from.
	 * @return A  that contains values of type TSource selected from the input
	 * sequence.
	 */
	static function ToHashSet<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.collections.generic.HashSet<TSource>;
	/**
	 * Creates a  from an .
	 * @param TSource The type of the elements of .
	 * @param source The  to create a  from.
	 * @return A  that contains elements from the input sequence.
	 */
	static function ToList<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.List<TSource>;
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>):cs.system.linq.ILookup<TKey, TSource> {})
	@:overload(function<TSource, TKey>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ILookup<TKey, TSource> {})
	@:overload(function<TSource, TKey, TElement>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>):cs.system.linq.ILookup<TKey, TElement> {})
	/**
	 * Creates a  from an  according to a specified key selector function.
	 * @param TSource The type of the elements of .
	 * @param TKey The type of the key returned by .
	 * @param source The  to create a  from.
	 * @param keySelector A function to extract a key from each element.
	 * @return A  that contains keys and values.
	 */
	static function ToLookup<TSource, TKey, TElement>(source:cs.system.collections.generic.IEnumerable<TSource>, keySelector:cs.system.Func_2<TSource, TKey>, elementSelector:cs.system.Func_2<TSource, TElement>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):cs.system.linq.ILookup<TKey, TElement>;
	@:overload(function<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Produces the set union of two sequences by using the default equality comparer.
	 * @param TSource The type of the elements of the input sequences.
	 * @param first An  whose distinct elements form the first set for the union.
	 * @param second An  whose distinct elements form the second set for the union.
	 * @return An  that contains the elements from both input sequences, excluding
	 * duplicates.
	 */
	static function Union<TSource>(first:cs.system.collections.generic.IEnumerable<TSource>, second:cs.system.collections.generic.IEnumerable<TSource>, comparer:cs.system.collections.generic.IEqualityComparer<TSource>):cs.system.collections.generic.IEnumerable<TSource>;
	@:overload(function<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_2<TSource, Bool>):cs.system.collections.generic.IEnumerable<TSource> {})
	/**
	 * Filters a sequence of values based on a predicate.
	 * @param TSource The type of the elements of .
	 * @param source An  to filter.
	 * @param predicate A function to test each element for a condition.
	 * @return An  that contains elements from the input sequence that satisfy the
	 * condition.
	 */
	static function Where<TSource>(source:cs.system.collections.generic.IEnumerable<TSource>, predicate:cs.system.Func_3<TSource, Int, Bool>):cs.system.collections.generic.IEnumerable<TSource>;
	/**
	 * Applies a specified function to the corresponding elements of two sequences,
	 * producing a sequence of the results.
	 * @param TFirst The type of the elements of the first input sequence.
	 * @param TSecond The type of the elements of the second input sequence.
	 * @param TResult The type of the elements of the result sequence.
	 * @param first The first sequence to merge.
	 * @param second The second sequence to merge.
	 * @param resultSelector A function that specifies how to merge the elements from
	 * the two sequences.
	 * @return An  that contains merged elements of two input sequences.
	 */
	static function Zip<TFirst, TSecond, TResult>(first:cs.system.collections.generic.IEnumerable<TFirst>, second:cs.system.collections.generic.IEnumerable<TSecond>, resultSelector:cs.system.Func_3<TFirst, TSecond, TResult>):cs.system.collections.generic.IEnumerable<TResult>;
}
