package cs.system.linq.expressions;

/** Provides the base class from which the classes that represent expression tree nodes are derived. It also contains  ( in Visual Basic) factory methods to create the various node types. This is an  class. */
@:native("System.Linq.Expressions.Expression`1")
extern class Expression_1<TDelegate> extends cs.system.linq.expressions.LambdaExpression {
	@:overload(function():TDelegate {})
	@:overload(function(preferInterpretation:Bool):TDelegate {})
	function Compile(debugInfoGenerator:cs.system.runtime.compilerservices.DebugInfoGenerator):TDelegate;
	function Update(body:cs.system.linq.expressions.Expression, parameters:cs.system.collections.generic.IEnumerable<cs.system.linq.expressions.ParameterExpression>):cs.system.linq.expressions.Expression_1<TDelegate>;
}
