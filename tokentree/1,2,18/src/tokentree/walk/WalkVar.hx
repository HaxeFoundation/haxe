package tokentree.walk;

class WalkVar {
	public static function walkVar(stream:TokenStream, parent:TokenTree) {
		var name:Null<TokenTree> = null;
		var varTok:Null<TokenTree> = stream.consumeTokenDef(Kwd(KwdVar));
		parent.addChild(varTok);
		WalkComment.walkComment(stream, parent);
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			if (stream.tokenForMatch().match(Kwd(KwdVar))) {
				return;
			}
			WalkComment.walkComment(stream, parent);
			switch (stream.token()) {
				case At:
					WalkAt.walkAts(stream);
				default:
			}
			WalkComment.walkComment(stream, parent);
			var nameParent:TokenTree = varTok;
			if (stream.tokenForMatch().match(Question)) {
				nameParent = stream.consumeToken();
				varTok.addChild(nameParent);
			}
			name = stream.consumeConstIdent();
			nameParent.addChild(name);
			stream.applyTempStore(name);
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