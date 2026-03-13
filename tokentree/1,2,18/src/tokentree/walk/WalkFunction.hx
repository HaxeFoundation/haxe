package tokentree.walk;

class WalkFunction {
	public static function walkFunction(stream:TokenStream, parent:TokenTree) {
		var funcTok:Null<TokenTree> = stream.consumeTokenDef(Kwd(KwdFunction));
		parent.addChild(funcTok);
		WalkComment.walkComment(stream, funcTok);

		var name:TokenTree = funcTok;
		switch (stream.token()) {
			case Kwd(KwdNew):
				name = WalkTypeNameDef.walkTypeNameDef(stream, funcTok);
			case POpen:
			case Binop(OpLt):
				WalkLtGt.walkLtGt(stream, funcTok);
				name = funcTok.getLastChild();
			default:
				name = WalkTypeNameDef.walkTypeNameDef(stream, funcTok);
		}
		stream.applyTempStore(name);
		WalkComment.walkComment(stream, name);
		WalkFunction.walkFunctionParameters(stream, name);
		WalkComment.walkComment(stream, name);

		switch (stream.token()) {
			case Sharp(_):
				WalkSharp.walkSharp(stream, name, WalkStatement.walkStatement);
				switch (stream.token()) {
					case DblDot | BrOpen:
					default: return;
				}
			default:
		}

		if (stream.tokenForMatch().match(DblDot)) {
			var dblDot:Null<TokenTree> = stream.consumeToken();
			name.addChild(dblDot);
			WalkTypeNameDef.walkTypeNameDef(stream, dblDot);
		}
		WalkBlock.walkBlock(stream, name);
	}

	static function walkFunctionParameters(stream:TokenStream, parent:TokenTree) {
		var pOpen:TokenTree = stream.consumeTokenDef(POpen);
		parent.addChild(pOpen);
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			WalkComment.walkComment(stream, pOpen);
			if (stream.tokenForMatch().match(PClose)) break;
			WalkFieldDef.walkFieldDef(stream, pOpen);
		}
		pOpen.addChild(stream.consumeTokenDef(PClose));
	}
}