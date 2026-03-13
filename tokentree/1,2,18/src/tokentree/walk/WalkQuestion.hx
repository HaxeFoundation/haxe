package tokentree.walk;

class WalkQuestion {
	public static function walkQuestion(stream:TokenStream, parent:TokenTree) {
		var ternary:Bool = isTernary(stream, parent);
		if (!ternary) {
			WalkFieldDef.walkFieldDef(stream, parent);
			return;
		}
		var question:TokenTree = stream.consumeTokenDef(Question);
		parent.addChild(question);
		WalkComment.walkComment(stream, question);
		WalkStatement.walkStatement(stream, question);
		WalkComment.walkComment(stream, question);
		if (!stream.tokenForMatch().match(DblDot)) return;
		var dblDotTok:TokenTree = stream.consumeTokenDef(DblDot);
		question.addChild(dblDotTok);
		WalkStatement.walkStatement(stream, dblDotTok);
	}

	public static function isTernary(stream:TokenStream, parent:TokenTree):Bool {
		var lastChild:Null<TokenTree> = parent.getLastChild();
		if (lastChild == null) {
			return switch (parent.tok) {
				case Const(_): true;
				case Kwd(KwdTrue), Kwd(KwdFalse), Kwd(KwdNull): true;
				case Kwd(KwdThis): true;
				case Dollar(_): true;
				case PClose: true;
				default: false;
			}
		}
		return switch (lastChild.tok) {
			case Const(_): true;
			case BkOpen: true;
			case BrOpen: true;
			case Binop(OpAdd), Binop(OpSub): true;
			case Unop(_): true;
			case Kwd(KwdCast): true;
			case Kwd(KwdNew): true;
			case Kwd(KwdTrue), Kwd(KwdFalse), Kwd(KwdNull): true;
			case Kwd(KwdMacro): (lastChild.index + 1 != stream.getStreamIndex());
			case Kwd(KwdThis), Kwd(KwdUntyped): true;
			case Kwd(KwdFunction): true;
			case Dollar(_): true;
			case POpen: true;
			case PClose: true;
			case DblDot: true;
			default: false;
		}
	}
}