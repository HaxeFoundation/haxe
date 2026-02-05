package cs.system.linq.expressions;

/** Represents initializing the elements of a collection member of a newly created object. */
@:native("System.Linq.Expressions.MemberListBinding")
extern class MemberListBinding extends cs.system.linq.expressions.MemberBinding {
	/**
	 * Gets the element initializers for initializing a collection member of a newly
	 * created object.
	 * @return A  of  objects to initialize a collection member with.
	 */
	var Initializers(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.ElementInit>;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param initializers The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.MemberListBinding;
}
