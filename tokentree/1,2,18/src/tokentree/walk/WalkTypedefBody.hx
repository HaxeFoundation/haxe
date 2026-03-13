package tokentree.walk;

class WalkTypedefBody {
	public static function walkTypedefBody(stream:TokenStream, parent:TokenTree) {
		if (stream.tokenForMatch().match(BrOpen)) {
			var openTok:TokenTree = stream.consumeToken();
			parent.addChild(openTok);
			walkTypedefCurlyBody(stream, openTok);
			openTok.addChild(stream.consumeTokenDef(BrClose));
		}
		else {
			walkTypedefAlias(stream, parent);
		}
		if (stream.tokenForMatch().match(Binop(OpAnd))) {
			var and:TokenTree = stream.consumeTokenDef(Binop(OpAnd));
			parent.getLastChild().addChild(and);
			walkTypedefBody(stream, parent);
		}
		if (stream.tokenForMatch().match(Arrow)) {
			WalkStatement.walkStatement(stream, parent);
		}
	}

	public static function walkTypedefCurlyBody(stream:TokenStream, openTok:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case At:
					stream.addToTempStore(WalkAt.walkAt(stream));
				case BrClose:
					break;
				case Sharp(_):
					WalkSharp.walkSharp(stream, openTok, WalkTypedefBody.walkTypedefCurlyBody);
				case Binop(OpGt):
					walkStructureExtension(stream, openTok);
				case Comment(_), CommentLine(_):
					if (stream.hasTempStore()) {
						stream.consumeToTempStore();
					}
					else {
						WalkComment.walkComment(stream, openTok);
					}
				case Kwd(KwdFunction):
					WalkFunction.walkFunction(stream, openTok);
				case Kwd(KwdVar):
					WalkVar.walkVar(stream, openTok);
				case Kwd(KwdPublic), Kwd(KwdPrivate), Kwd(KwdStatic), Kwd(KwdInline), Kwd(KwdMacro), Kwd(KwdOverride), Kwd(KwdDynamic), Kwd(KwdExtern):
					stream.consumeToTempStore();
				case Kwd(KwdFinal):
					WalkFinal.walkFinal(stream, openTok);
				default:
					WalkFieldDef.walkFieldDef(stream, openTok);
			}
		}
		var tempStore:Array<TokenTree> = stream.getTempStore();
		if (tempStore.length > 0) {
			switch (TokenStream.MODE) {
				case Relaxed:
					stream.applyTempStore(openTok);
				case Strict:
					throw "invalid token tree structure - found:" + '$tempStore';
			}
		}
	}

	public static function walkTypedefAlias(stream:TokenStream, parent:TokenTree) {
		var newParent:TokenTree;
		if (stream.tokenForMatch().match(POpen)) {
			newParent = WalkPOpen.walkPOpen(stream, parent);
		}
		else {
			newParent = WalkTypeNameDef.walkTypeNameDef(stream, parent);
		}
		if (stream.tokenForMatch().match(Arrow)) {
			var arrowTok:TokenTree = stream.consumeToken();
			newParent.addChild(arrowTok);
			walkTypedefAlias(stream, arrowTok);
		}
		if (stream.tokenForMatch().match(Semicolon)) {
			newParent.addChild(stream.consumeToken());
		}
	}

	static function walkStructureExtension(stream:TokenStream, parent:TokenTree) {
		var gt:TokenTree = stream.consumeTokenDef(Binop(OpGt));
		parent.addChild(gt);
		var name:TokenTree = WalkTypeNameDef.walkTypeNameDef(stream, parent);
		gt.addChild(name);
		if (stream.tokenForMatch().match(Comma)) {
			name.addChild(stream.consumeToken());
		}
	}
}