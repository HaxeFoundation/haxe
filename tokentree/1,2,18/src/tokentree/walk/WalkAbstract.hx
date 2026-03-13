package tokentree.walk;

class WalkAbstract {
	public static function walkAbstract(stream:TokenStream, parent:TokenTree) {
		var typeTok:TokenTree = stream.consumeToken();
		switch (stream.token()) {
			case Kwd(KwdClass) | Kwd(KwdInterface):
				stream.addToTempStore(typeTok);
				WalkType.walkType(stream, parent);
				return;
			default:
		}
		parent.addChild(typeTok);
		var name:TokenTree = WalkTypeNameDef.walkTypeNameDef(stream, typeTok);
		// add all comments, annotations
		stream.applyTempStore(name);
		if (stream.tokenForMatch().match(POpen)) WalkPOpen.walkPOpen(stream, name);
		var typeParent:TokenTree = name;
		var typeChild:TokenTree;
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case BrOpen:
					break;
				case Comment(_), CommentLine(_):
					name.addChild(stream.consumeToken());
				case Const(CIdent("from")), Const(CIdent("to")):
					var fromToken:TokenTree = stream.consumeToken();
					name.addChild(fromToken);
					WalkTypeNameDef.walkTypeNameDef(stream, fromToken);
				default:
					typeChild = stream.consumeToken();
					typeParent.addChild(typeChild);
					typeParent = typeChild;
			}
		}
		var block:TokenTree = stream.consumeTokenDef(BrOpen);
		name.addChild(block);
		WalkAbstract.walkAbstractBody(stream, block);
		block.addChild(stream.consumeTokenDef(BrClose));
	}

	public static function walkAbstractBody(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case Kwd(KwdVar):
					WalkVar.walkVar(stream, parent);
				case Kwd(KwdFunction):
					WalkFunction.walkFunction(stream, parent);
				case Kwd(KwdFinal):
					WalkFinal.walkFinal(stream, parent);
				case Sharp(_):
					WalkSharp.walkSharp(stream, parent, WalkAbstract.walkAbstractBody);
				case At:
					stream.addToTempStore(WalkAt.walkAt(stream));
				case BrClose:
					break;
				case Semicolon:
					parent.addChild(stream.consumeToken());
				case Comment(_), CommentLine(_):
					parent.addChild(stream.consumeToken());
				default:
					stream.consumeToTempStore();
			}
		}
		stream.applyTempStore(parent);
	}
}