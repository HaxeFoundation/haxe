package cs.system.linq.expressions;

/** Represents initializing members of a member of a newly created object. */
@:native("System.Linq.Expressions.MemberMemberBinding")
extern class MemberMemberBinding extends cs.system.linq.expressions.MemberBinding {
	/**
	 * Gets the bindings that describe how to initialize the members of a member.
	 * @return A  of  objects that describe how to initialize the members of the
	 * member.
	 */
	var Bindings(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.MemberBinding>;
	/**
	 * Creates a new expression that is like this one, but using the supplied children.
	 * If all of the children are the same, it will return this expression.
	 * @param bindings The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(bindings:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberMemberBinding;
}
