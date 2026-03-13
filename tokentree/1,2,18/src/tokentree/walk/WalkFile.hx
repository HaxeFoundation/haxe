package tokentree.walk;

class WalkFile {
	public static function walkFile(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (stream.hasMore() && progress.streamHasChanged()) {
			switch (stream.token()) {
				case Kwd(KwdPackage) | Kwd(KwdImport) | Kwd(KwdUsing):
					stream.applyTempStore(parent);
					WalkPackageImport.walkPackageImport(stream, parent);
				case Sharp(_):
					WalkSharp.walkSharp(stream, parent, WalkFile.walkFile);
					if (!stream.hasMore()) return;
					switch (stream.token()) {
						case BrOpen: WalkBlock.walkBlock(stream, parent.children[parent.children.length - 1]);
						default:
					}
				case At:
					stream.addToTempStore(WalkAt.walkAt(stream));
				case Comment(_) | CommentLine(_):
					if (stream.hasTempStore()) {
						stream.consumeToTempStore();
					}
					else {
						WalkComment.walkComment(stream, parent);
					}
				case Kwd(KwdClass) | Kwd(KwdInterface) | Kwd(KwdEnum) | Kwd(KwdTypedef) | Kwd(KwdAbstract):
					WalkType.walkType(stream, parent);
				case PClose, BrClose, BkClose, Semicolon, Comma:
					parent.addChild(stream.consumeToken());
				case Kwd(KwdPublic) | Kwd(KwdPrivate) | Kwd(KwdStatic) | Kwd(KwdInline) | Kwd(KwdMacro) | Kwd(KwdDynamic) | Kwd(KwdExtern) | Kwd(KwdOverload):
					stream.consumeToTempStore();
				case Kwd(KwdFinal):
					WalkFinal.walkFinal(stream, parent);
				case Kwd(KwdVar):
					WalkVar.walkVar(stream, parent);
				case Kwd(KwdFunction):
					WalkFunction.walkFunction(stream, parent);
				default:
					WalkBlock.walkBlock(stream, parent);
			}
		}
		var tempStore:Array<TokenTree> = stream.getTempStore();
		for (stored in tempStore) {
			switch (stored.tok) {
				case Kwd(KwdExtern) | Kwd(KwdPrivate) | Kwd(KwdPublic) | At:
					switch (TokenStream.MODE) {
						case Relaxed: parent.addChild(stored);
						case Strict: throw "invalid token tree structure - found:" + stored;
					}
				default:
					parent.addChild(stored);
			}
		}
	}
}