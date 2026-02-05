package cs.system.linq.expressions;

/** Represents a dynamic operation. */
@:native("System.Linq.Expressions.DynamicExpression")
extern class DynamicExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the arguments to the dynamic operation.
	 * @return The read-only collections containing the arguments to the dynamic
	 * operation.
	 */
	var Arguments(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.linq.expressions.Expression>;
	/**
	 * Gets the , which determines the run-time behavior of the dynamic site.
	 * @return The , which determines the run-time behavior of the dynamic site.
	 */
	var Binder(default, never):cs.system.runtime.compilerservices.CallSiteBinder;
	/**
	 * Gets the type of the delegate used by the .
	 * @return The  object representing the type of the delegate used by the .
	 */
	var DelegateType(default, never):cs.system.Type;
	@:overload(function(binder:cs.system.runtime.compilerservices.CallSiteBinder, returnType:cs.system.Type, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(binder:cs.system.runtime.compilerservices.CallSiteBinder, returnType:cs.system.Type, arg0:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(binder:cs.system.runtime.compilerservices.CallSiteBinder, returnType:cs.system.Type, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(binder:cs.system.runtime.compilerservices.CallSiteBinder, returnType:cs.system.Type, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(binder:cs.system.runtime.compilerservices.CallSiteBinder, returnType:cs.system.Type, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression {})
	/**
	 * Creates a  that represents a dynamic operation bound by the provided .
	 * @param binder The runtime binder for the dynamic operation.
	 * @param returnType The result type of the dynamic expression.
	 * @param arguments The arguments to the dynamic operation.
	 * @return A  that has  equal to ,  and has the  and  set to the specified values.
	 */
	static function Dynamic(binder:cs.system.runtime.compilerservices.CallSiteBinder, returnType:cs.system.Type, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression;
	@:overload(function(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder, arg0:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression {})
	@:overload(function(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression {})
	/**
	 * Creates a  that represents a dynamic operation bound by the provided .
	 * @param delegateType The type of the delegate used by the .
	 * @param binder The runtime binder for the dynamic operation.
	 * @param arguments The arguments to the dynamic operation.
	 * @return A  that has  equal to , and has the , , and  set to the specified
	 * values.
	 */
	static function MakeDynamic(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression;
	/**
	 * Compares the value sent to the parameter, , to the  property of the current
	 * instance of . If the values of the parameter and the property are equal, the
	 * current instance is returned. If they are not equal, a new  instance is returned
	 * that is identical to the current instance except that the  property is set to
	 * the value of parameter .
	 * @param arguments The  property of the result.
	 * @return This expression if no children are changed or an expression with the
	 * updated children.
	 */
	function Update(arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.DynamicExpression;
}
