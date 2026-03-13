package tokentree.walk;

class WalkBlock {
	/**
	 * BrOpen
	 *  |- statement
	 *  |- statement
	 *  |- BrClose
	 *
	 */
	public static function walkBlock(stream:TokenStream, parent:TokenTree) {
		while (stream.tokenForMatch().match(At)) stream.addToTempStore(WalkAt.walkAt(stream));
		if (stream.tokenForMatch().match(BrOpen)) {
			var openTok:TokenTree = stream.consumeTokenDef(BrOpen);
			parent.addChild(openTok);
			stream.applyTempStore(openTok);
			walkBlockContinue(stream, openTok);
			stream.applyTempStore(openTok);
		}
		else {
			WalkStatement.walkStatement(stream, parent);
		}
	}

	public static function walkBlockContinue(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case BrClose:
					break;
				case Comma:
					var child:TokenTree = stream.consumeToken();
					var lastChild:Null<TokenTree> = parent.getLastChild();
					if (lastChild == null) {
						parent.addChild(child);
					}
					else {
						lastChild.addChild(child);
					}
				case BkClose, PClose:
					var child:TokenTree = stream.consumeToken();
					parent.addChild(child);
				case Kwd(KwdCase), Kwd(KwdDefault):
					WalkSwitch.walkSwitchCases(stream, parent);
				default:
					WalkStatement.walkStatement(stream, parent);
			}
		}
		walkBlockEnd(stream, parent);
	}

	public static function walkBlockEnd(stream:TokenStream, parent:TokenTree) {
		parent.addChild(stream.consumeTokenDef(BrClose));
		if (stream.hasMore()) {
			switch (stream.token()) {
				case Binop(OpGt):
					return;
				default:
			}
			walkAfterBlock(stream, parent);
			if (stream.hasMore()) {
				switch (stream.token()) {
					case Semicolon:
						var semicolon:TokenTree = stream.consumeToken();
						parent.addChild(semicolon);
					default:
				}
			}
		}
	}

	@:access(tokentree.walk.WalkStatement)
	static function walkAfterBlock(stream:TokenStream, parent:TokenTree) {
		if (!stream.hasMore()) return;
		switch (stream.token()) {
			case Dot:
				WalkStatement.walkStatementWithoutSemicolon(stream, parent);
			case DblDot:
				WalkStatement.walkDblDot(stream, parent);
			case Semicolon:
				return;
			case Arrow:
				WalkStatement.walkStatementWithoutSemicolon(stream, parent);
			case Binop(_):
				WalkStatement.walkStatementWithoutSemicolon(stream, parent);
			case Const(CIdent("is")):
				WalkStatement.walkStatementWithoutSemicolon(stream, parent);
			case Unop(_):
				if (parent.isCIdentOrCString()) {
					WalkStatement.walkStatementWithoutSemicolon(stream, parent);
				}
			case Question:
				WalkQuestion.walkQuestion(stream, parent);
			case POpen:
				switch (parent.parent.tok) {
					case Dollar(_): WalkStatement.walkStatementWithoutSemicolon(stream, parent);
					default:
				}
			case CommentLine(_), Comment(_):
				var nextTokDef:Null<TokenTreeDef> = stream.peekNonCommentToken();
				if (nextTokDef == null) {
					return;
				}
				switch (nextTokDef) {
					case Dot, DblDot, Binop(_), Unop(_), Question:
						WalkComment.walkComment(stream, parent);
						WalkStatement.walkStatementContinue(stream, parent);
					default:
				}
			default:
		}
	}
}