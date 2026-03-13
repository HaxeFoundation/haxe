package tokentree.walk;

class WalkInterface {
	public static function walkInterface(stream:TokenStream, parent:TokenTree) {
		var typeTok:TokenTree = stream.consumeToken();
		parent.addChild(typeTok);
		// add name
		var name:TokenTree = WalkTypeNameDef.walkTypeNameDef(stream, typeTok);
		// add all comments, annotations
		stream.applyTempStore(name);
		WalkClass.walkClassExtends(stream, name);
		var block:TokenTree = stream.consumeTokenDef(BrOpen);
		name.addChild(block);
		WalkInterface.walkInterfaceBody(stream, block);
		block.addChild(stream.consumeTokenDef(BrClose));
	}

	public static function walkInterfaceBody(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case Kwd(KwdVar):
					WalkVar.walkVar(stream, parent);
				case Kwd(KwdFunction):
					WalkFunction.walkFunction(stream, parent);
				case Sharp(_):
					WalkSharp.walkSharp(stream, parent, WalkInterface.walkInterfaceBody);
				case At:
					stream.addToTempStore(WalkAt.walkAt(stream));
				case BrClose:
					break;
				case Semicolon:
					parent.addChild(stream.consumeToken());
				case Kwd(KwdPublic), Kwd(KwdPrivate), Kwd(KwdStatic), Kwd(KwdInline), Kwd(KwdMacro), Kwd(KwdOverride), Kwd(KwdDynamic), Kwd(KwdExtern):
					stream.consumeToTempStore();
				case Kwd(KwdFinal):
					WalkFinal.walkFinal(stream, parent);
				case Comment(_), CommentLine(_):
					if (stream.hasTempStore()) {
						stream.consumeToTempStore();
					}
					else {
						parent.addChild(stream.consumeToken());
					}
				default:
					stream.consumeToTempStore();
			}
		}
		stream.applyTempStore(parent);
	}
}