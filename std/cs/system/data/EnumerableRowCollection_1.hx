package cs.system.data;

@:native("System.Data.EnumerableRowCollection`1")
extern class EnumerableRowCollection_1<TRow> extends cs.system.data.EnumerableRowCollection {
	function GetEnumerator():cs.system.collections.generic.IEnumerator<TRow>;
}
