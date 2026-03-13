package tokentree.walk;

import tokentree.TokenTreeAccessHelper;

class WalkClass {
	public static function walkClass(stream:TokenStream, parent:TokenTree) {
		var typeTok:TokenTree = stream.consumeToken();
		parent.addChild(typeTok);
		WalkComment.walkComment(stream, parent);
		var name:TokenTree = typeTok;
		switch (stream.token()) {
			case Const(CIdent(_)):
				name = WalkTypeNameDef.walkTypeNameDef(stream, typeTok);
				// add all comments, annotations
				stream.applyTempStore(name);
			case Dollar(_):
				name = WalkTypeNameDef.walkTypeNameDef(stream, typeTok);
				stream.applyTempStore(name);
			default:
		}
		WalkClass.walkClassExtends(stream, name);
		var block:TokenTree = stream.consumeTokenDef(BrOpen);
		name.addChild(block);
		WalkClass.walkClassBody(stream, block);
		block.addChild(stream.consumeTokenDef(BrClose));
	}

	public static function walkClassExtends(stream:TokenStream, name:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			WalkExtends.walkExtends(stream, name);
			WalkImplements.walkImplements(stream, name);
			if (stream.isSharp()) WalkSharp.walkSharp(stream, name, WalkClass.walkClassExtends);
			WalkComment.walkComment(stream, name);
		}
	}

	public static function walkClassBody(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (stream.hasMore() && progress.streamHasChanged()) {
			switch (stream.token()) {
				case Kwd(KwdVar):
					WalkVar.walkVar(stream, parent);
				case Kwd(KwdFunction):
					WalkFunction.walkFunction(stream, parent);
				case Sharp(_):
					WalkSharp.walkSharp(stream, parent, WalkClass.walkClassBody);
					walkClassContinueAfterSharp(stream, parent);
				case At:
					stream.addToTempStore(WalkAt.walkAt(stream));
				case BrClose:
					break;
				case Semicolon:
					parent.addChild(stream.consumeToken());
				case Kwd(KwdPublic) | Kwd(KwdPrivate) | Kwd(KwdStatic) | Kwd(KwdInline) | Kwd(KwdMacro) | Kwd(KwdOverride) | Kwd(KwdDynamic) |
					Kwd(KwdExtern) | Kwd(KwdAbstract) | Kwd(KwdOverload):
					stream.consumeToTempStore();
				case Kwd(KwdFinal):
					WalkFinal.walkFinal(stream, parent);
				case Comment(_) | CommentLine(_):
					if (stream.hasTempStore()) {
						stream.consumeToTempStore();
					}
					else {
						parent.addChild(stream.consumeToken());
					}
				default:
					switch (TokenStream.MODE) {
						case Relaxed: WalkStatement.walkStatement(stream, parent);
						case Strict: throw "invalid token tree structure - found:" + '${stream.token()}';
					}
			}
		}
		var tempStore:Array<TokenTree> = stream.getTempStore();
		if (tempStore.length > 0) {
			switch (TokenStream.MODE) {
				case Relaxed:
					stream.applyTempStore(parent);
				case Strict:
					throw "invalid token tree structure - found:" + '$tempStore';
			}
		}
	}

	static function walkClassContinueAfterSharp(stream:TokenStream, parent:TokenTree) {
		@:nullSafety(Off)
		var brOpen:TokenTreeAccessHelper = TokenTreeAccessHelper.access(parent).lastChild().matches(Sharp("if")).lastOf(Kwd(KwdFunction)).firstChild()
			.lastChild().matches(BrOpen);

		if (brOpen.token == null) return;

		@:nullSafety(Off)
		if (brOpen.lastChild().matches(BrClose).token != null) return;

		WalkBlock.walkBlockContinue(stream, parent);
	}
}