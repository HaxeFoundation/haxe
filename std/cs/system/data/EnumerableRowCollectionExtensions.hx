package cs.system.data;

@:native("System.Data.EnumerableRowCollectionExtensions")
extern class EnumerableRowCollectionExtensions {
	static function Cast<TResult>(source:cs.system.data.EnumerableRowCollection):cs.system.data.EnumerableRowCollection_1<TResult>;
	@:overload(function<TRow, TKey>(source:cs.system.data.EnumerableRowCollection_1<TRow>, keySelector:cs.system.Func_2<TRow, TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow> {})
	static function OrderBy<TRow, TKey>(source:cs.system.data.EnumerableRowCollection_1<TRow>, keySelector:cs.system.Func_2<TRow, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow>;
	@:overload(function<TRow, TKey>(source:cs.system.data.EnumerableRowCollection_1<TRow>, keySelector:cs.system.Func_2<TRow, TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow> {})
	static function OrderByDescending<TRow, TKey>(source:cs.system.data.EnumerableRowCollection_1<TRow>, keySelector:cs.system.Func_2<TRow, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow>;
	static function Select<TRow, S>(source:cs.system.data.EnumerableRowCollection_1<TRow>, selector:cs.system.Func_2<TRow, S>):cs.system.data.EnumerableRowCollection_1<S>;
	@:overload(function<TRow, TKey>(source:cs.system.data.OrderedEnumerableRowCollection<TRow>, keySelector:cs.system.Func_2<TRow, TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow> {})
	static function ThenBy<TRow, TKey>(source:cs.system.data.OrderedEnumerableRowCollection<TRow>, keySelector:cs.system.Func_2<TRow, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow>;
	@:overload(function<TRow, TKey>(source:cs.system.data.OrderedEnumerableRowCollection<TRow>, keySelector:cs.system.Func_2<TRow, TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow> {})
	static function ThenByDescending<TRow, TKey>(source:cs.system.data.OrderedEnumerableRowCollection<TRow>, keySelector:cs.system.Func_2<TRow, TKey>, comparer:cs.system.collections.generic.IComparer<TKey>):cs.system.data.OrderedEnumerableRowCollection<TRow>;
	static function Where<TRow>(source:cs.system.data.EnumerableRowCollection_1<TRow>, predicate:cs.system.Func_2<TRow, Bool>):cs.system.data.EnumerableRowCollection_1<TRow>;
}
