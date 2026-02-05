package cs.system.dynamic;

/** Represents a set of binding restrictions on the  under which the dynamic binding is valid. */
@:native("System.Dynamic.BindingRestrictions")
extern class BindingRestrictions {
	/** Represents an empty set of binding restrictions. This field is read only. */
	static var Empty(default, never):cs.system.dynamic.BindingRestrictions;
	/**
	 * Combines binding restrictions from the list of  instances into one set of
	 * restrictions.
	 * @param contributingObjects The list of  instances from which to combine
	 * restrictions.
	 * @return The new set of binding restrictions.
	 */
	static function Combine(contributingObjects:cs.system.collections.generic.IList<cs.system.dynamic.DynamicMetaObject>):cs.system.dynamic.BindingRestrictions;
	/**
	 * Creates the binding restriction that checks the expression for arbitrary
	 * immutable properties.
	 * @param expression The expression representing the restrictions.
	 * @return The new binding restrictions.
	 */
	static function GetExpressionRestriction(expression:cs.system.linq.expressions.Expression):cs.system.dynamic.BindingRestrictions;
	/**
	 * Creates the binding restriction that checks the expression for object instance
	 * identity.
	 * @param expression The expression to test.
	 * @param instance The exact object instance to test.
	 * @return The new binding restrictions.
	 */
	static function GetInstanceRestriction(expression:cs.system.linq.expressions.Expression, instance:Dynamic):cs.system.dynamic.BindingRestrictions;
	/**
	 * Creates the binding restriction that check the expression for runtime type
	 * identity.
	 * @param expression The expression to test.
	 * @param type The exact type to test.
	 * @return The new binding restrictions.
	 */
	static function GetTypeRestriction(expression:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.dynamic.BindingRestrictions;
	/**
	 * Merges the set of binding restrictions with the current binding restrictions.
	 * @param restrictions The set of restrictions with which to merge the current
	 * binding restrictions.
	 * @return The new set of binding restrictions.
	 */
	function Merge(restrictions:cs.system.dynamic.BindingRestrictions):cs.system.dynamic.BindingRestrictions;
	/**
	 * Creates the  representing the binding restrictions.
	 * @return The expression tree representing the restrictions.
	 */
	function ToExpression():cs.system.linq.expressions.Expression;
}
