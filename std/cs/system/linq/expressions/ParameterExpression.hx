package cs.system.linq.expressions;

/** Represents a named parameter expression. */
@:native("System.Linq.Expressions.ParameterExpression")
extern class ParameterExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Indicates that this ParameterExpression is to be treated as a  parameter.
	 * @return if this ParameterExpression is a  parameter; otherwise, .
	 */
	var IsByRef(default, never):Bool;
	/**
	 * Gets the name of the parameter or variable.
	 * @return A  that contains the name of the parameter.
	 */
	var Name(default, never):String;
}
