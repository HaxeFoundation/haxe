package cs.system.linq;

/** Represents an expression tree and provides functionality to execute the expression tree after rewriting it. */
@:native("System.Linq.EnumerableExecutor`1")
extern class EnumerableExecutor_1<T> extends cs.system.linq.EnumerableExecutor {
	function new(expression:cs.system.linq.expressions.Expression):Void;
}
