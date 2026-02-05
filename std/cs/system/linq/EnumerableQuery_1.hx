package cs.system.linq;

/** Represents an  as an  data source. */
@:native("System.Linq.EnumerableQuery`1")
extern class EnumerableQuery_1<T> extends cs.system.linq.EnumerableQuery {
	@:overload(function(enumerable:cs.system.collections.generic.IEnumerable<T>):Void {})
	function new(expression:cs.system.linq.expressions.Expression):Void;
	function ToString():String;
}
