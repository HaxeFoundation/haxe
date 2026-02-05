package cs.system.linq;

/** Defines methods to create and execute queries that are described by an  object. */
@:native("System.Linq.IQueryProvider")
extern interface IQueryProvider {
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.IQueryable {})
	/**
	 * Constructs an  object that can evaluate the query represented by a specified
	 * expression tree.
	 * @param expression An expression tree that represents a LINQ query.
	 * @return An  that can evaluate the query represented by the specified expression
	 * tree.
	 */
	function CreateQuery<TElement>(expression:cs.system.linq.expressions.Expression):cs.system.linq.IQueryable_1<TElement>;
	@:overload(function(expression:cs.system.linq.expressions.Expression):Dynamic {})
	/**
	 * Executes the query represented by a specified expression tree.
	 * @param expression An expression tree that represents a LINQ query.
	 * @return The value that results from executing the specified query.
	 */
	function Execute<TResult>(expression:cs.system.linq.expressions.Expression):TResult;
}
