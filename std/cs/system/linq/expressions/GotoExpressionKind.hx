package cs.system.linq.expressions;

/** Specifies what kind of jump this  represents. */
@:native("System.Linq.Expressions.GotoExpressionKind")
extern enum abstract GotoExpressionKind(Int) {
	var Break = 2;
	var Continue = 3;
	var Goto = 0;
	var Return = 1;
}
