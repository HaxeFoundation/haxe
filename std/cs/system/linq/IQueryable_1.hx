package cs.system.linq;

/** Provides functionality to evaluate queries against a specific data source wherein the type of the data is not specified. */
@:native("System.Linq.IQueryable`1")
extern interface IQueryable_1<T> extends cs.system.collections.generic.IEnumerable<T> extends cs.system.collections.IEnumerable extends cs.system.linq.IQueryable {
}
