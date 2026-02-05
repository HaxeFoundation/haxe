package cs.system.linq;

/** Provides functionality to evaluate queries against a specific data source wherein the type of the data is not specified. */
@:native("System.Linq.IQueryable")
extern interface IQueryable extends cs.system.collections.IEnumerable {
	/**
	 * Gets the type of the element(s) that are returned when the expression tree
	 * associated with this instance of  is executed.
	 * @return A  that represents the type of the element(s) that are returned when the
	 * expression tree associated with this object is executed.
	 */
	var ElementType(default, never):cs.system.Type;
	/**
	 * Gets the expression tree that is associated with the instance of .
	 * @return The  that is associated with this instance of .
	 */
	var Expression(default, never):cs.system.linq.expressions.Expression;
	/**
	 * Gets the query provider that is associated with this data source.
	 * @return The  that is associated with this data source.
	 */
	var Provider(default, never):cs.system.linq.IQueryProvider;
}
