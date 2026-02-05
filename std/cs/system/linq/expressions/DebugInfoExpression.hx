package cs.system.linq.expressions;

/** Emits or clears a sequence point for debug information. This allows the debugger to highlight the correct source code when debugging. */
@:native("System.Linq.Expressions.DebugInfoExpression")
extern class DebugInfoExpression extends cs.system.linq.expressions.Expression {
	/**
	 * Gets the  that represents the source file.
	 * @return The  that represents the source file.
	 */
	var Document(default, never):cs.system.linq.expressions.SymbolDocumentInfo;
	/**
	 * Gets the end column of this .
	 * @return The number of the end column of the code that was used to generate the
	 * wrapped expression.
	 */
	var EndColumn(default, never):Int;
	/**
	 * Gets the end line of this .
	 * @return The number of the end line of the code that was used to generate the
	 * wrapped expression.
	 */
	var EndLine(default, never):Int;
	/**
	 * Gets the value to indicate if the  is for clearing a sequence point.
	 * @return if the  is for clearing a sequence point; otherwise, .
	 */
	var IsClear(default, never):Bool;
	/**
	 * Gets the start column of this .
	 * @return The number of the start column of the code that was used to generate the
	 * wrapped expression.
	 */
	var StartColumn(default, never):Int;
	/**
	 * Gets the start line of this .
	 * @return The number of the start line of the code that was used to generate the
	 * wrapped expression.
	 */
	var StartLine(default, never):Int;
}
