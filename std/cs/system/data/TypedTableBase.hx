package cs.system.data;

@:native("System.Data.TypedTableBase")
extern class TypedTableBase<T> extends cs.system.data.DataTable {
	function Cast<TResult>():cs.system.data.EnumerableRowCollection_1<TResult>;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
}
