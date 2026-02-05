package cs.system.linq.expressions;

/** Provides the base class from which the classes that represent expression tree nodes are derived. It also contains  ( in Visual Basic) factory methods to create the various node types. This is an  class. */
@:native("System.Linq.Expressions.Expression")
extern class Expression {
	/**
	 * Indicates that the node can be reduced to a simpler node. If this returns true,
	 * Reduce() can be called to produce the reduced form.
	 * @return if the node can be reduced; otherwise, .
	 */
	var CanReduce(default, never):Bool;
	/**
	 * Gets the node type of this .
	 * @return One of the  values.
	 */
	var NodeType(default, never):cs.system.linq.expressions.ExpressionType;
	/**
	 * Gets the static type of the expression that this  represents.
	 * @return The  that represents the static type of the expression.
	 */
	var Type(default, never):cs.system.Type;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic addition operation that does not have
	 * overflow checking.
	 * @param left A  to set the  property equal to.
	 * @param right A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Add(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an addition assignment operation that does not have
	 * overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function AddAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an addition assignment operation that has overflow
	 * checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function AddAssignChecked(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic addition operation that has overflow
	 * checking.
	 * @param left A  to set the  property equal to.
	 * @param right A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function AddChecked(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise  operation.
	 * @param left A  to set the  property equal to.
	 * @param right A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function And(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a conditional  operation that evaluates the second
	 * operand only if the first operand evaluates to .
	 * @param left A  to set the  property equal to.
	 * @param right A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function AndAlso(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise AND assignment operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function AndAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(array:cs.system.linq.expressions.Expression, indexes:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.IndexExpression {})
	/**
	 * Creates an  to access a multidimensional array.
	 * @param array An expression that represents the multidimensional array.
	 * @param indexes An  containing expressions used to index the array.
	 * @return The created .
	 */
	static function ArrayAccess(array:cs.system.linq.expressions.Expression, indexes:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.IndexExpression;
	@:overload(function(array:cs.system.linq.expressions.Expression, indexes:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(array:cs.system.linq.expressions.Expression, index:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents applying an array index operator to an array of rank
	 * more than one.
	 * @param array An  to set the  property equal to.
	 * @param indexes An  that contains  objects to use to populate the  collection.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ArrayIndex(array:cs.system.linq.expressions.Expression, indexes:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression;
	/**
	 * Creates a  that represents an expression for obtaining the length of a
	 * one-dimensional array.
	 * @param array An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property equal to .
	 */
	static function ArrayLength(array:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression;
	/**
	 * Creates a  that represents an assignment operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Assign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(member:cs.system.reflection.MemberInfo, expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MemberAssignment {})
	/**
	 * Creates a  that represents the initialization of a field or property.
	 * @param member A  to set the  property equal to.
	 * @param expression An  to set the  property equal to.
	 * @return A  that has  equal to  and the  and  properties set to the specified
	 * values.
	 */
	static function Bind(propertyAccessor:cs.system.reflection.MethodInfo, expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MemberAssignment;
	@:overload(function(expressions:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(expressions:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(variables:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>, expressions:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(variables:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>, expressions:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(type:cs.system.Type, expressions:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(type:cs.system.Type, expressions:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(type:cs.system.Type, variables:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>, expressions:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(type:cs.system.Type, variables:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>, expressions:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.BlockExpression {})
	@:overload(function(arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BlockExpression {})
	/**
	 * Creates a  that contains the given expressions and has no variables.
	 * @param expressions The expressions in the block.
	 * @return The created .
	 */
	static function Block(arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression, arg4:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BlockExpression;
	@:overload(function(target:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.GotoExpression {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression):cs.system.linq.expressions.GotoExpression {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget, type:cs.system.Type):cs.system.linq.expressions.GotoExpression {})
	/**
	 * Creates a  representing a break statement.
	 * @param target The  that the  will jump to.
	 * @return A  with  equal to Break, the  property set to , and a null value to be
	 * passed to the target label upon jumping.
	 */
	static function Break(target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.GotoExpression;
	@:overload(function(instance:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(method:cs.system.reflection.MethodInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(method:cs.system.reflection.MethodInfo, arg0:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(method:cs.system.reflection.MethodInfo, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(method:cs.system.reflection.MethodInfo, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, methodName:String, typeArguments:cs.NativeArray<cs.system.Type>, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(method:cs.system.reflection.MethodInfo, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(type:cs.system.Type, methodName:String, typeArguments:cs.NativeArray<cs.system.Type>, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MethodCallExpression {})
	@:overload(function(method:cs.system.reflection.MethodInfo, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MethodCallExpression {})
	/**
	 * Creates a  that represents a call to a method that takes no arguments.
	 * @param instance An  that specifies the instance for an instance method call
	 * (pass  for a  ( in Visual Basic) method).
	 * @param method A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Call(method:cs.system.reflection.MethodInfo, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression, arg4:cs.system.linq.expressions.Expression):cs.system.linq.expressions.MethodCallExpression;
	@:overload(function(variable:cs.system.linq.expressions.ParameterExpression, body:cs.system.linq.expressions.Expression):cs.system.linq.expressions.CatchBlock {})
	@:overload(function(type:cs.system.Type, body:cs.system.linq.expressions.Expression):cs.system.linq.expressions.CatchBlock {})
	@:overload(function(variable:cs.system.linq.expressions.ParameterExpression, body:cs.system.linq.expressions.Expression, filter:cs.system.linq.expressions.Expression):cs.system.linq.expressions.CatchBlock {})
	/**
	 * Creates a  representing a catch statement with a reference to the caught  object
	 * for use in the handler body.
	 * @param variable A  representing a reference to the  object caught by this
	 * handler.
	 * @param body The body of the catch statement.
	 * @return The created .
	 */
	static function Catch(type:cs.system.Type, body:cs.system.linq.expressions.Expression, filter:cs.system.linq.expressions.Expression):cs.system.linq.expressions.CatchBlock;
	/**
	 * Creates a  for clearing a sequence point.
	 * @param document The  that represents the source file.
	 * @return An instance of  for clearing a sequence point.
	 */
	static function ClearDebugInfo(document:cs.system.linq.expressions.SymbolDocumentInfo):cs.system.linq.expressions.DebugInfoExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a coalescing operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Coalesce(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(test:cs.system.linq.expressions.Expression, ifTrue:cs.system.linq.expressions.Expression, ifFalse:cs.system.linq.expressions.Expression):cs.system.linq.expressions.ConditionalExpression {})
	/**
	 * Creates a  that represents a conditional statement.
	 * @param test An  to set the  property equal to.
	 * @param ifTrue An  to set the  property equal to.
	 * @param ifFalse An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the , , and  properties set to
	 * the specified values.
	 */
	static function Condition(test:cs.system.linq.expressions.Expression, ifTrue:cs.system.linq.expressions.Expression, ifFalse:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.ConditionalExpression;
	@:overload(function(value:Dynamic):cs.system.linq.expressions.ConstantExpression {})
	/**
	 * Creates a  that has the  property set to the specified value.
	 * @param value An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function Constant(value:Dynamic, type:cs.system.Type):cs.system.linq.expressions.ConstantExpression;
	@:overload(function(target:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.GotoExpression {})
	/**
	 * Creates a  representing a continue statement.
	 * @param target The  that the  will jump to.
	 * @return A  with  equal to Continue, the  property set to , and a null value to
	 * be passed to the target label upon jumping.
	 */
	static function Continue(target:cs.system.linq.expressions.LabelTarget, type:cs.system.Type):cs.system.linq.expressions.GotoExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents a type conversion operation.
	 * @param expression An  to set the  property equal to.
	 * @param type A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Convert(expression:cs.system.linq.expressions.Expression, type:cs.system.Type, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents a conversion operation that throws an exception if
	 * the target type is overflowed.
	 * @param expression An  to set the  property equal to.
	 * @param type A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ConvertChecked(expression:cs.system.linq.expressions.Expression, type:cs.system.Type, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	/**
	 * Creates a  with the specified span.
	 * @param document The  that represents the source file.
	 * @param startLine The start line of this . Must be greater than 0.
	 * @param startColumn The start column of this . Must be greater than 0.
	 * @param endLine The end line of this . Must be greater or equal than the start
	 * line.
	 * @param endColumn The end column of this . If the end line is the same as the
	 * start line, it must be greater or equal than the start column. In any case, must
	 * be greater than 0.
	 * @return An instance of .
	 */
	static function DebugInfo(document:cs.system.linq.expressions.SymbolDocumentInfo, startLine:Int, startColumn:Int, endLine:Int, endColumn:Int):cs.system.linq.expressions.DebugInfoExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents the decrementing of the expression by 1.
	 * @param expression An  to decrement.
	 * @return A  that represents the decremented expression.
	 */
	static function Decrement(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	/**
	 * Creates a  that has the  property set to the specified type.
	 * @param type A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified type.
	 */
	static function Default(type:cs.system.Type):cs.system.linq.expressions.DefaultExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic division operation.
	 * @param left An  to set the  property to.
	 * @param right An  to set the  property to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Divide(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a division assignment operation that does not have
	 * overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function DivideAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
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
	 * @return A  that has  equal to  and has the  and  set to the specified values.
	 */
	static function Dynamic(binder:cs.system.runtime.compilerservices.CallSiteBinder, returnType:cs.system.Type, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression;
	@:overload(function(addMethod:cs.system.reflection.MethodInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.ElementInit {})
	/**
	 * Creates an , given an  as the second argument.
	 * @param addMethod A  to set the  property equal to.
	 * @param arguments An  that contains  objects to set the  property equal to.
	 * @return An  that has the  and  properties set to the specified values.
	 */
	static function ElementInit(addMethod:cs.system.reflection.MethodInfo, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.ElementInit;
	/**
	 * Creates an empty expression that has  type.
	 * @return A  that has the  property equal to  and the  property set to .
	 */
	static function Empty():cs.system.linq.expressions.DefaultExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an equality comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Equal(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise  operation, using op_ExclusiveOr for
	 * user-defined types.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ExclusiveOr(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise XOR assignment operation, using
	 * op_ExclusiveOr for user-defined types.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ExclusiveOrAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression, field:cs.system.reflection.FieldInfo):cs.system.linq.expressions.MemberExpression {})
	@:overload(function(expression:cs.system.linq.expressions.Expression, fieldName:String):cs.system.linq.expressions.MemberExpression {})
	/**
	 * Creates a  that represents accessing a field.
	 * @param expression An  to set the  property equal to. For  ( in Visual Basic), 
	 * must be .
	 * @param field The  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Field(expression:cs.system.linq.expressions.Expression, type:cs.system.Type, fieldName:String):cs.system.linq.expressions.MemberExpression;
	/**
	 * Creates a  object that represents a generic System.Action delegate type that has
	 * specific type arguments.
	 * @param typeArgs An array of up to sixteen  objects that specify the type
	 * arguments for the  delegate type.
	 * @return The type of a System.Action delegate that has the specified type
	 * arguments.
	 */
	static function GetActionType(typeArgs:cs.NativeArray<cs.system.Type>):cs.system.Type;
	/**
	 * Gets a  object that represents a generic System.Func or System.Action delegate
	 * type that has specific type arguments.
	 * @param typeArgs The type arguments of the delegate.
	 * @return The delegate type.
	 */
	static function GetDelegateType(typeArgs:cs.NativeArray<cs.system.Type>):cs.system.Type;
	/**
	 * Creates a  object that represents a generic System.Func delegate type that has
	 * specific type arguments. The last type argument specifies the return type of the
	 * created delegate.
	 * @param typeArgs An array of one to seventeen  objects that specify the type
	 * arguments for the  delegate type.
	 * @return The type of a System.Func delegate that has the specified type
	 * arguments.
	 */
	static function GetFuncType(typeArgs:cs.NativeArray<cs.system.Type>):cs.system.Type;
	@:overload(function(target:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.GotoExpression {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression):cs.system.linq.expressions.GotoExpression {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget, type:cs.system.Type):cs.system.linq.expressions.GotoExpression {})
	/**
	 * Creates a  representing a "go to" statement.
	 * @param target The  that the  will jump to.
	 * @return A  with  equal to Goto, the  property set to the specified value, and a
	 * null value to be passed to the target label upon jumping.
	 */
	static function Goto(target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.GotoExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a "greater than" numeric comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function GreaterThan(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a "greater than or equal" numeric comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function GreaterThanOrEqual(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	/**
	 * Creates a  that represents a conditional block with an  statement.
	 * @param test An  to set the  property equal to.
	 * @param ifTrue An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the , , properties set to the
	 * specified values. The  property is set to default expression and the type of the
	 * resulting  returned by this method is .
	 */
	static function IfThen(test:cs.system.linq.expressions.Expression, ifTrue:cs.system.linq.expressions.Expression):cs.system.linq.expressions.ConditionalExpression;
	/**
	 * Creates a  that represents a conditional block with  and  statements.
	 * @param test An  to set the  property equal to.
	 * @param ifTrue An  to set the  property equal to.
	 * @param ifFalse An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the , , and  properties set to
	 * the specified values. The type of the resulting  returned by this method is .
	 */
	static function IfThenElse(test:cs.system.linq.expressions.Expression, ifTrue:cs.system.linq.expressions.Expression, ifFalse:cs.system.linq.expressions.Expression):cs.system.linq.expressions.ConditionalExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents the incrementing of the expression value by 1.
	 * @param expression An  to increment.
	 * @return A  that represents the incremented expression.
	 */
	static function Increment(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.InvocationExpression {})
	/**
	 * Creates an  that applies a delegate or lambda expression to a list of argument
	 * expressions.
	 * @param expression An  that represents the delegate or lambda expression to be
	 * applied to.
	 * @param arguments An  that contains  objects that represent the arguments that
	 * the delegate or lambda expression is applied to.
	 * @return An  that applies the specified delegate or lambda expression to the
	 * provided arguments.
	 */
	static function Invoke(expression:cs.system.linq.expressions.Expression, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.InvocationExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Returns whether the expression evaluates to false.
	 * @param expression An  to evaluate.
	 * @return An instance of .
	 */
	static function IsFalse(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Returns whether the expression evaluates to true.
	 * @param expression An  to evaluate.
	 * @return An instance of .
	 */
	static function IsTrue(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function():cs.system.linq.expressions.LabelTarget {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.LabelExpression {})
	@:overload(function(name:String):cs.system.linq.expressions.LabelTarget {})
	@:overload(function(type:cs.system.Type):cs.system.linq.expressions.LabelTarget {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget, defaultValue:cs.system.linq.expressions.Expression):cs.system.linq.expressions.LabelExpression {})
	/**
	 * Creates a  representing a label with void type and no name.
	 * @return The new .
	 */
	static function Label(type:cs.system.Type, name:String):cs.system.linq.expressions.LabelTarget;
	@:overload(function(body:cs.system.linq.expressions.Expression, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(body:cs.system.linq.expressions.Expression, parameters:cs.NativeArray<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function<TDelegate>(body:cs.system.linq.expressions.Expression, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.Expression_1<TDelegate> {})
	@:overload(function<TDelegate>(body:cs.system.linq.expressions.Expression, parameters:cs.NativeArray<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.Expression_1<TDelegate> {})
	@:overload(function(body:cs.system.linq.expressions.Expression, tailCall:Bool, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(body:cs.system.linq.expressions.Expression, tailCall:Bool, parameters:cs.NativeArray<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(body:cs.system.linq.expressions.Expression, name:String, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(delegateType:cs.system.Type, body:cs.system.linq.expressions.Expression, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(delegateType:cs.system.Type, body:cs.system.linq.expressions.Expression, parameters:cs.NativeArray<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function<TDelegate>(body:cs.system.linq.expressions.Expression, tailCall:Bool, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.Expression_1<TDelegate> {})
	@:overload(function<TDelegate>(body:cs.system.linq.expressions.Expression, tailCall:Bool, parameters:cs.NativeArray<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.Expression_1<TDelegate> {})
	@:overload(function<TDelegate>(body:cs.system.linq.expressions.Expression, name:String, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.Expression_1<TDelegate> {})
	@:overload(function(body:cs.system.linq.expressions.Expression, name:String, tailCall:Bool, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(delegateType:cs.system.Type, body:cs.system.linq.expressions.Expression, tailCall:Bool, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(delegateType:cs.system.Type, body:cs.system.linq.expressions.Expression, tailCall:Bool, parameters:cs.NativeArray<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function(delegateType:cs.system.Type, body:cs.system.linq.expressions.Expression, name:String, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression {})
	@:overload(function<TDelegate>(body:cs.system.linq.expressions.Expression, name:String, tailCall:Bool, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.Expression_1<TDelegate> {})
	/**
	 * Creates a  by first constructing a delegate type from the expression body, a
	 * parameter that indicates whether tail call optimization will be applied, and an
	 * enumerable collection of parameter expressions. It can be used when the delegate
	 * type is not known at compile time.
	 * @param body An  to set the  property equal to.
	 * @param tailCall A  that indicates if tail call optimization will be applied when
	 * compiling the created expression.
	 * @param parameters An  that contains  objects to use to populate the  collection.
	 * @return A  that has the  property equal to Lambda and the  and  properties set
	 * to the specified values.
	 */
	static function Lambda(delegateType:cs.system.Type, body:cs.system.linq.expressions.Expression, name:String, tailCall:Bool, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.LambdaExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise left-shift operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function LeftShift(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise left-shift assignment operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function LeftShiftAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a "less than" numeric comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function LessThan(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a " less than or equal" numeric comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function LessThanOrEqual(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(member:cs.system.reflection.MemberInfo, initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.MemberListBinding {})
	@:overload(function(member:cs.system.reflection.MemberInfo, initializers:cs.NativeArray<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.MemberListBinding {})
	@:overload(function(propertyAccessor:cs.system.reflection.MethodInfo, initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.MemberListBinding {})
	/**
	 * Creates a  where the member is a field or property.
	 * @param member A  that represents a field or property to set the  property equal
	 * to.
	 * @param initializers An  that contains  objects to use to populate the 
	 * collection.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ListBind(propertyAccessor:cs.system.reflection.MethodInfo, initializers:cs.NativeArray<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.MemberListBinding;
	@:overload(function(newExpression:cs.system.linq.expressions.NewExpression, initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.ListInitExpression {})
	@:overload(function(newExpression:cs.system.linq.expressions.NewExpression, initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.ListInitExpression {})
	@:overload(function(newExpression:cs.system.linq.expressions.NewExpression, initializers:cs.NativeArray<cs.system.linq.expressions.ElementInit>):cs.system.linq.expressions.ListInitExpression {})
	@:overload(function(newExpression:cs.system.linq.expressions.NewExpression, initializers:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.ListInitExpression {})
	@:overload(function(newExpression:cs.system.linq.expressions.NewExpression, addMethod:cs.system.reflection.MethodInfo, initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.ListInitExpression {})
	/**
	 * Creates a  that uses specified  objects to initialize a collection.
	 * @param newExpression A  to set the  property equal to.
	 * @param initializers An  that contains  objects to use to populate the 
	 * collection.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ListInit(newExpression:cs.system.linq.expressions.NewExpression, addMethod:cs.system.reflection.MethodInfo, initializers:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.ListInitExpression;
	@:overload(function(body:cs.system.linq.expressions.Expression):cs.system.linq.expressions.LoopExpression {})
	@:overload(function(body:cs.system.linq.expressions.Expression, break_:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.LoopExpression {})
	/**
	 * Creates a  with the given body.
	 * @param body The body of the loop.
	 * @return The created .
	 */
	static function Loop(body:cs.system.linq.expressions.Expression, break_:cs.system.linq.expressions.LabelTarget, continue_:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.LoopExpression;
	@:overload(function(binaryType:cs.system.linq.expressions.ExpressionType, left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(binaryType:cs.system.linq.expressions.ExpressionType, left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a , given the left and right operands, by calling an appropriate factory
	 * method.
	 * @param binaryType The  that specifies the type of binary operation.
	 * @param left An  that represents the left operand.
	 * @param right An  that represents the right operand.
	 * @return The  that results from calling the appropriate factory method.
	 */
	static function MakeBinary(binaryType:cs.system.linq.expressions.ExpressionType, left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	/**
	 * Creates a  representing a catch statement with the specified elements.
	 * @param type The  of  this  will handle.
	 * @param variable A  representing a reference to the  object caught by this
	 * handler.
	 * @param body The body of the catch statement.
	 * @param filter The body of the  filter.
	 * @return The created .
	 */
	static function MakeCatchBlock(type:cs.system.Type, variable:cs.system.linq.expressions.ParameterExpression, body:cs.system.linq.expressions.Expression, filter:cs.system.linq.expressions.Expression):cs.system.linq.expressions.CatchBlock;
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
	 * @return A  that has  equal to  and has the , , and  set to the specified values.
	 */
	static function MakeDynamic(delegateType:cs.system.Type, binder:cs.system.runtime.compilerservices.CallSiteBinder, arg0:cs.system.linq.expressions.Expression, arg1:cs.system.linq.expressions.Expression, arg2:cs.system.linq.expressions.Expression, arg3:cs.system.linq.expressions.Expression):cs.system.linq.expressions.DynamicExpression;
	/**
	 * Creates a  representing a jump of the specified . The value passed to the label
	 * upon jumping can also be specified.
	 * @param kind The  of the .
	 * @param target The  that the  will jump to.
	 * @param value The value that will be passed to the associated label upon jumping.
	 * @param type An  to set the  property equal to.
	 * @return A  with  equal to , the  property set to , the  property set to , and 
	 * to be passed to the target label upon jumping.
	 */
	static function MakeGoto(kind:cs.system.linq.expressions.GotoExpressionKind, target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.GotoExpression;
	/**
	 * Creates an  that represents accessing an indexed property in an object.
	 * @param instance The object to which the property belongs. It should be null if
	 * the property is  ( in Visual Basic).
	 * @param indexer An  representing the property to index.
	 * @param arguments An IEnumerable<Expression> (IEnumerable (Of Expression) in
	 * Visual Basic) that contains the arguments that will be used to index the
	 * property.
	 * @return The created .
	 */
	static function MakeIndex(instance:cs.system.linq.expressions.Expression, indexer:cs.system.reflection.PropertyInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.IndexExpression;
	/**
	 * Creates a  that represents accessing either a field or a property.
	 * @param expression An  that represents the object that the member belongs to.
	 * This can be null for static members.
	 * @param member The  that describes the field or property to be accessed.
	 * @return The  that results from calling the appropriate factory method.
	 */
	static function MakeMemberAccess(expression:cs.system.linq.expressions.Expression, member:cs.system.reflection.MemberInfo):cs.system.linq.expressions.MemberExpression;
	/**
	 * Creates a  representing a try block with the specified elements.
	 * @param type The result type of the try expression. If null, body and all
	 * handlers must have identical type.
	 * @param body The body of the try block.
	 * @param finally The body of the finally block. Pass null if the try block has no
	 * finally block associated with it.
	 * @param fault The body of the fault block. Pass null if the try block has no
	 * fault block associated with it.
	 * @param handlers A collection of s representing the catch statements to be
	 * associated with the try block.
	 * @return The created .
	 */
	static function MakeTry(type:cs.system.Type, body:cs.system.linq.expressions.Expression, finally:cs.system.linq.expressions.Expression, fault:cs.system.linq.expressions.Expression, handlers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.CatchBlock>):cs.system.linq.expressions.TryExpression;
	@:overload(function(unaryType:cs.system.linq.expressions.ExpressionType, operand:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a , given an operand, by calling the appropriate factory method.
	 * @param unaryType The  that specifies the type of unary operation.
	 * @param operand An  that represents the operand.
	 * @param type The  that specifies the type to be converted to (pass  if not
	 * applicable).
	 * @return The  that results from calling the appropriate factory method.
	 */
	static function MakeUnary(unaryType:cs.system.linq.expressions.ExpressionType, operand:cs.system.linq.expressions.Expression, type:cs.system.Type, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(member:cs.system.reflection.MemberInfo, bindings:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberMemberBinding {})
	@:overload(function(member:cs.system.reflection.MemberInfo, bindings:cs.NativeArray<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberMemberBinding {})
	@:overload(function(propertyAccessor:cs.system.reflection.MethodInfo, bindings:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberMemberBinding {})
	/**
	 * Creates a  that represents the recursive initialization of members of a field or
	 * property.
	 * @param member The  to set the  property equal to.
	 * @param bindings An  that contains  objects to use to populate the  collection.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function MemberBind(propertyAccessor:cs.system.reflection.MethodInfo, bindings:cs.NativeArray<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberMemberBinding;
	@:overload(function(newExpression:cs.system.linq.expressions.NewExpression, bindings:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberInitExpression {})
	/**
	 * Represents an expression that creates a new object and initializes a property of
	 * the object.
	 * @param newExpression A  to set the  property equal to.
	 * @param bindings An  that contains  objects to use to populate the  collection.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function MemberInit(newExpression:cs.system.linq.expressions.NewExpression, bindings:cs.NativeArray<cs.system.linq.expressions.MemberBinding>):cs.system.linq.expressions.MemberInitExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic remainder operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Modulo(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a remainder assignment operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ModuloAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic multiplication operation that does not
	 * have overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Multiply(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a multiplication assignment operation that does not
	 * have overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function MultiplyAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a multiplication assignment operation that has
	 * overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function MultiplyAssignChecked(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic multiplication operation that has
	 * overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function MultiplyChecked(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents an arithmetic negation operation.
	 * @param expression An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function Negate(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents an arithmetic negation operation that has overflow
	 * checking.
	 * @param expression An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function NegateChecked(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(constructor:cs.system.reflection.ConstructorInfo):cs.system.linq.expressions.NewExpression {})
	@:overload(function(type:cs.system.Type):cs.system.linq.expressions.NewExpression {})
	@:overload(function(constructor:cs.system.reflection.ConstructorInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewExpression {})
	@:overload(function(constructor:cs.system.reflection.ConstructorInfo, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewExpression {})
	@:overload(function(constructor:cs.system.reflection.ConstructorInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>, members:cs.system.collections.generic.IEnumerable<cs.system.reflection.MemberInfo>):cs.system.linq.expressions.NewExpression {})
	/**
	 * Creates a  that represents calling the specified constructor that takes no
	 * arguments.
	 * @param constructor The  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function New(constructor:cs.system.reflection.ConstructorInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>, members:cs.NativeArray<cs.system.reflection.MemberInfo>):cs.system.linq.expressions.NewExpression;
	@:overload(function(type:cs.system.Type, bounds:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewArrayExpression {})
	/**
	 * Creates a  that represents creating an array that has a specified rank.
	 * @param type A  that represents the element type of the array.
	 * @param bounds An  that contains  objects to use to populate the  collection.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function NewArrayBounds(type:cs.system.Type, bounds:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewArrayExpression;
	@:overload(function(type:cs.system.Type, initializers:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewArrayExpression {})
	/**
	 * Creates a  that represents creating a one-dimensional array and initializing it
	 * from a list of elements.
	 * @param type A  that represents the element type of the array.
	 * @param initializers An  that contains  objects to use to populate the 
	 * collection.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function NewArrayInit(type:cs.system.Type, initializers:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.NewArrayExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents a bitwise complement operation.
	 * @param expression An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function Not(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an inequality comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function NotEqual(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, liftToNull:Bool, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Returns the expression representing the ones complement.
	 * @param expression An .
	 * @return An instance of .
	 */
	static function OnesComplement(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise  operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Or(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise OR assignment operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function OrAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a conditional  operation that evaluates the second
	 * operand only if the first operand evaluates to .
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function OrElse(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(type:cs.system.Type):cs.system.linq.expressions.ParameterExpression {})
	/**
	 * Creates a  node that can be used to identify a parameter or a variable in an
	 * expression tree.
	 * @param type The type of the parameter or variable.
	 * @return A  node with the specified name and type.
	 */
	static function Parameter(type:cs.system.Type, name:String):cs.system.linq.expressions.ParameterExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents the assignment of the expression followed by a
	 * subsequent decrement by 1 of the original expression.
	 * @param expression An  to apply the operations on.
	 * @return A  that represents the resultant expression.
	 */
	static function PostDecrementAssign(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents the assignment of the expression followed by a
	 * subsequent increment by 1 of the original expression.
	 * @param expression An  to apply the operations on.
	 * @return A  that represents the resultant expression.
	 */
	static function PostIncrementAssign(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents raising a number to a power.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Power(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents raising an expression to a power and assigning the
	 * result back to the expression.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function PowerAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that decrements the expression by 1 and assigns the result back to
	 * the expression.
	 * @param expression An  to apply the operations on.
	 * @return A  that represents the resultant expression.
	 */
	static function PreDecrementAssign(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that increments the expression by 1 and assigns the result back to
	 * the expression.
	 * @param expression An  to apply the operations on.
	 * @return A  that represents the resultant expression.
	 */
	static function PreIncrementAssign(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression, propertyAccessor:cs.system.reflection.MethodInfo):cs.system.linq.expressions.MemberExpression {})
	@:overload(function(expression:cs.system.linq.expressions.Expression, property:cs.system.reflection.PropertyInfo):cs.system.linq.expressions.MemberExpression {})
	@:overload(function(expression:cs.system.linq.expressions.Expression, propertyName:String):cs.system.linq.expressions.MemberExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, indexer:cs.system.reflection.PropertyInfo, arguments:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.IndexExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, indexer:cs.system.reflection.PropertyInfo, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.IndexExpression {})
	@:overload(function(instance:cs.system.linq.expressions.Expression, propertyName:String, arguments:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.IndexExpression {})
	/**
	 * Creates a  that represents accessing a property by using a property accessor
	 * method.
	 * @param expression An  to set the  property equal to. This can be null for static
	 * properties.
	 * @param propertyAccessor The  that represents a property accessor method.
	 * @return A  that has the  property equal to , the  property set to  and the 
	 * property set to the  that represents the property accessed in .
	 */
	static function Property(expression:cs.system.linq.expressions.Expression, type:cs.system.Type, propertyName:String):cs.system.linq.expressions.MemberExpression;
	/**
	 * Creates a  that represents accessing a property or field.
	 * @param expression An  whose  contains a property or field named . This can be
	 * null for static members.
	 * @param propertyOrFieldName The name of a property or field to be accessed.
	 * @return A  that has the  property equal to , the  property set to , and the 
	 * property set to the  or  that represents the property or field denoted by .
	 */
	static function PropertyOrField(expression:cs.system.linq.expressions.Expression, propertyOrFieldName:String):cs.system.linq.expressions.MemberExpression;
	/**
	 * Creates a  that represents an expression that has a constant value of type .
	 * @param expression An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function Quote(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression;
	/**
	 * Creates a  that represents a reference equality comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ReferenceEqual(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression;
	/**
	 * Creates a  that represents a reference inequality comparison.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function ReferenceNotEqual(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function():cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents a rethrowing of an exception.
	 * @return A  that represents a rethrowing of an exception.
	 */
	static function Rethrow(type:cs.system.Type):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(target:cs.system.linq.expressions.LabelTarget):cs.system.linq.expressions.GotoExpression {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression):cs.system.linq.expressions.GotoExpression {})
	@:overload(function(target:cs.system.linq.expressions.LabelTarget, type:cs.system.Type):cs.system.linq.expressions.GotoExpression {})
	/**
	 * Creates a  representing a return statement.
	 * @param target The  that the  will jump to.
	 * @return A  with  equal to Return, the  property set to , and a null value to be
	 * passed to the target label upon jumping.
	 */
	static function Return(target:cs.system.linq.expressions.LabelTarget, value:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.GotoExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise right-shift operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function RightShift(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a bitwise right-shift assignment operation.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function RightShiftAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(variables:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.RuntimeVariablesExpression {})
	/**
	 * Creates an instance of .
	 * @param variables A collection of  objects to use to populate the  collection.
	 * @return An instance of  that has the  property equal to  and the  property set
	 * to the specified value.
	 */
	static function RuntimeVariables(variables:cs.NativeArray<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.RuntimeVariablesExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic subtraction operation that does not
	 * have overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function Subtract(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a subtraction assignment operation that does not have
	 * overflow checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function SubtractAssign(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents a subtraction assignment operation that has overflow
	 * checking.
	 * @param left An  to set the  property equal to.
	 * @param right An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function SubtractAssignChecked(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo, conversion:cs.system.linq.expressions.LambdaExpression):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression):cs.system.linq.expressions.BinaryExpression {})
	/**
	 * Creates a  that represents an arithmetic subtraction operation that has overflow
	 * checking.
	 * @param left A  to set the  property equal to.
	 * @param right A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function SubtractChecked(left:cs.system.linq.expressions.Expression, right:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.BinaryExpression;
	@:overload(function(switchValue:cs.system.linq.expressions.Expression, cases:cs.NativeArray<cs.system.linq.expressions.SwitchCase>):cs.system.linq.expressions.SwitchExpression {})
	@:overload(function(switchValue:cs.system.linq.expressions.Expression, defaultBody:cs.system.linq.expressions.Expression, cases:cs.NativeArray<cs.system.linq.expressions.SwitchCase>):cs.system.linq.expressions.SwitchExpression {})
	@:overload(function(switchValue:cs.system.linq.expressions.Expression, defaultBody:cs.system.linq.expressions.Expression, comparison:cs.system.reflection.MethodInfo, cases:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.SwitchCase>):cs.system.linq.expressions.SwitchExpression {})
	@:overload(function(switchValue:cs.system.linq.expressions.Expression, defaultBody:cs.system.linq.expressions.Expression, comparison:cs.system.reflection.MethodInfo, cases:cs.NativeArray<cs.system.linq.expressions.SwitchCase>):cs.system.linq.expressions.SwitchExpression {})
	@:overload(function(type:cs.system.Type, switchValue:cs.system.linq.expressions.Expression, defaultBody:cs.system.linq.expressions.Expression, comparison:cs.system.reflection.MethodInfo, cases:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.SwitchCase>):cs.system.linq.expressions.SwitchExpression {})
	/**
	 * Creates a  that represents a  statement that has a default case.
	 * @param switchValue The value to be tested against each case.
	 * @param defaultBody The result of the switch if  does not match any of the cases.
	 * @param cases The set of cases for this switch expression.
	 * @return The created .
	 */
	static function Switch(type:cs.system.Type, switchValue:cs.system.linq.expressions.Expression, defaultBody:cs.system.linq.expressions.Expression, comparison:cs.system.reflection.MethodInfo, cases:cs.NativeArray<cs.system.linq.expressions.SwitchCase>):cs.system.linq.expressions.SwitchExpression;
	@:overload(function(body:cs.system.linq.expressions.Expression, testValues:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.SwitchCase {})
	/**
	 * Creates a  object to be used in a  object.
	 * @param body The body of the case.
	 * @param testValues The test values of the case.
	 * @return The created .
	 */
	static function SwitchCase(body:cs.system.linq.expressions.Expression, testValues:cs.NativeArray<cs.system.linq.expressions.Expression>):cs.system.linq.expressions.SwitchCase;
	@:overload(function(fileName:String):cs.system.linq.expressions.SymbolDocumentInfo {})
	@:overload(function(fileName:String, language:cs.system.Guid):cs.system.linq.expressions.SymbolDocumentInfo {})
	@:overload(function(fileName:String, language:cs.system.Guid, languageVendor:cs.system.Guid):cs.system.linq.expressions.SymbolDocumentInfo {})
	/**
	 * Creates an instance of .
	 * @param fileName A  to set the  equal to.
	 * @return A  that has the  property set to the specified value.
	 */
	static function SymbolDocument(fileName:String, language:cs.system.Guid, languageVendor:cs.system.Guid, documentType:cs.system.Guid):cs.system.linq.expressions.SymbolDocumentInfo;
	@:overload(function(value:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents a throwing of an exception.
	 * @param value An .
	 * @return A  that represents the exception.
	 */
	static function Throw(value:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.UnaryExpression;
	/**
	 * Creates a  representing a try block with any number of catch statements and
	 * neither a fault nor finally block.
	 * @param body The body of the try block.
	 * @param handlers The array of zero or more  expressions representing the catch
	 * statements to be associated with the try block.
	 * @return The created .
	 */
	static function TryCatch(body:cs.system.linq.expressions.Expression, handlers:cs.NativeArray<cs.system.linq.expressions.CatchBlock>):cs.system.linq.expressions.TryExpression;
	/**
	 * Creates a  representing a try block with any number of catch statements and a
	 * finally block.
	 * @param body The body of the try block.
	 * @param finally The body of the finally block.
	 * @param handlers The array of zero or more  expressions representing the catch
	 * statements to be associated with the try block.
	 * @return The created .
	 */
	static function TryCatchFinally(body:cs.system.linq.expressions.Expression, finally:cs.system.linq.expressions.Expression, handlers:cs.NativeArray<cs.system.linq.expressions.CatchBlock>):cs.system.linq.expressions.TryExpression;
	/**
	 * Creates a  representing a try block with a fault block and no catch statements.
	 * @param body The body of the try block.
	 * @param fault The body of the fault block.
	 * @return The created .
	 */
	static function TryFault(body:cs.system.linq.expressions.Expression, fault:cs.system.linq.expressions.Expression):cs.system.linq.expressions.TryExpression;
	/**
	 * Creates a  representing a try block with a finally block and no catch
	 * statements.
	 * @param body The body of the try block.
	 * @param finally The body of the finally block.
	 * @return The created .
	 */
	static function TryFinally(body:cs.system.linq.expressions.Expression, finally:cs.system.linq.expressions.Expression):cs.system.linq.expressions.TryExpression;
	/**
	 * Creates a  object that represents a generic System.Action delegate type that has
	 * specific type arguments.
	 * @param typeArgs An array of Type objects that specify the type arguments for the
	 * System.Action delegate type.
	 * @param actionType When this method returns, contains the generic System.Action
	 * delegate type that has specific type arguments. Contains null if there is no
	 * generic System.Action delegate that matches the .This parameter is passed
	 * uninitialized.
	 * @return if generic System.Action delegate type was created for specific ;
	 * otherwise, .
	 */
	static function TryGetActionType(typeArgs:cs.NativeArray<cs.system.Type>, actionType:cs.Ref<cs.system.Type>):Bool;
	/**
	 * Creates a  object that represents a generic System.Func delegate type that has
	 * specific type arguments. The last type argument specifies the return type of the
	 * created delegate.
	 * @param typeArgs An array of Type objects that specify the type arguments for the
	 * System.Func delegate type.
	 * @param funcType When this method returns, contains the generic System.Func
	 * delegate type that has specific type arguments. Contains null if there is no
	 * generic System.Func delegate that matches the .This parameter is passed
	 * uninitialized.
	 * @return if generic System.Func delegate type was created for specific ;
	 * otherwise, .
	 */
	static function TryGetFuncType(typeArgs:cs.NativeArray<cs.system.Type>, funcType:cs.Ref<cs.system.Type>):Bool;
	/**
	 * Creates a  that represents an explicit reference or boxing conversion where  is
	 * supplied if the conversion fails.
	 * @param expression An  to set the  property equal to.
	 * @param type A  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  and  properties set to the
	 * specified values.
	 */
	static function TypeAs(expression:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.UnaryExpression;
	/**
	 * Creates a  that compares run-time type identity.
	 * @param expression An  to set the  property equal to.
	 * @param type A  to set the  property equal to.
	 * @return A  for which the  property is equal to  and for which the  and 
	 * properties are set to the specified values.
	 */
	static function TypeEqual(expression:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.TypeBinaryExpression;
	/**
	 * Creates a .
	 * @param expression An  to set the  property equal to.
	 * @param type A  to set the  property equal to.
	 * @return A  for which the  property is equal to  and for which the  and 
	 * properties are set to the specified values.
	 */
	static function TypeIs(expression:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.TypeBinaryExpression;
	@:overload(function(expression:cs.system.linq.expressions.Expression):cs.system.linq.expressions.UnaryExpression {})
	/**
	 * Creates a  that represents a unary plus operation.
	 * @param expression An  to set the  property equal to.
	 * @return A  that has the  property equal to  and the  property set to the
	 * specified value.
	 */
	static function UnaryPlus(expression:cs.system.linq.expressions.Expression, method:cs.system.reflection.MethodInfo):cs.system.linq.expressions.UnaryExpression;
	/**
	 * Creates a  that represents an explicit unboxing.
	 * @param expression An  to unbox.
	 * @param type The new  of the expression.
	 * @return An instance of .
	 */
	static function Unbox(expression:cs.system.linq.expressions.Expression, type:cs.system.Type):cs.system.linq.expressions.UnaryExpression;
	@:overload(function(type:cs.system.Type):cs.system.linq.expressions.ParameterExpression {})
	/**
	 * Creates a  node that can be used to identify a parameter or a variable in an
	 * expression tree.
	 * @param type The type of the parameter or variable.
	 * @return A  node with the specified name and type
	 */
	static function Variable(type:cs.system.Type, name:String):cs.system.linq.expressions.ParameterExpression;
	/**
	 * Reduces this node to a simpler expression. If CanReduce returns true, this
	 * should return a valid expression. This method can return another node which
	 * itself must be reduced.
	 * @return The reduced expression.
	 */
	function Reduce():cs.system.linq.expressions.Expression;
	/**
	 * Reduces this node to a simpler expression. If CanReduce returns true, this
	 * should return a valid expression. This method can return another node which
	 * itself must be reduced.
	 * @return The reduced expression.
	 */
	function ReduceAndCheck():cs.system.linq.expressions.Expression;
	/**
	 * Reduces the expression to a known node type (that is not an Extension node) or
	 * just returns the expression if it is already a known type.
	 * @return The reduced expression.
	 */
	function ReduceExtensions():cs.system.linq.expressions.Expression;
	/**
	 * Returns a textual representation of the .
	 * @return A textual representation of the .
	 */
	function ToString():String;
}
