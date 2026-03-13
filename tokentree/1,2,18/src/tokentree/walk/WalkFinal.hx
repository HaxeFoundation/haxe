package tokentree.walk;

class WalkFinal {
	public static function walkFinal(stream:TokenStream, parent:TokenTree) {
		var name:Null<TokenTree> = null;
		var finalTok:TokenTree = stream.consumeToken();
		stream.addToTempStore(finalTok);
		WalkComment.walkComment(stream, parent);
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case Const(CIdent(_)):
					break;
				case Kwd(KwdPublic) | Kwd(KwdPrivate) | Kwd(KwdStatic) | Kwd(KwdInline) | Kwd(KwdMacro) | Kwd(KwdOverride) | Kwd(KwdDynamic) | Kwd(KwdExtern):
					stream.consumeToTempStore();
				case Comment(_) | CommentLine(_):
					stream.consumeToTempStore();
				case Kwd(KwdFunction):
					return;
				case Kwd(KwdClass) | Kwd(KwdInterface):
					return;
				default:
			}
		}
		parent.addChild(finalTok);

		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			WalkComment.walkComment(stream, parent);
			switch (stream.token()) {
				case At:
					WalkAt.walkAts(stream);
				default:
			}
			WalkComment.walkComment(stream, parent);
			var nameParent:TokenTree = finalTok;
			if (stream.tokenForMatch().match(Question)) {
				nameParent = stream.consumeToken();
				finalTok.addChild(nameParent);
			}
			name = stream.consumeConstIdent();
			nameParent.addChild(name);
			var tempStore:Array<TokenTree> = stream.getTempStore();
			for (stored in tempStore) {
				switch (stored.tok) {
					case Kwd(KwdFinal):
					default:
						name.addChild(stored);
				}
			}
			stream.clearTempStore();
			WalkComment.walkComment(stream, name);
			if (stream.tokenForMatch().match(POpen)) {
				WalkPOpen.walkPOpen(stream, name);
			}
			if (stream.tokenForMatch().match(DblDot)) {
				var dblDot:TokenTree = stream.consumeToken();
				name.addChild(dblDot);
				WalkTypedefBody.walkTypedefAlias(stream, dblDot);
			}
			if (stream.tokenForMatch().match(Binop(OpAssign))) {
				WalkStatement.walkStatement(stream, name);
			}
			if (stream.tokenForMatch().match(Comma)) {
				var comma:TokenTree = stream.consumeToken();
				name.addChild(comma);
				continue;
			}
			break;
		}

		if (stream.tokenForMatch().match(Semicolon)) {
			name.addChild(stream.consumeToken());
		}
	}
}