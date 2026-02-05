package cs.system.linq.expressions;

/** Specifies what kind of jump this  represents. */
@:native("System.Linq.Expressions.GotoExpressionKind")
extern enum GotoExpressionKind {
	Break;
	Continue;
	Goto;
	Return;
}
