package tokentree.walk;

class WalkPackageImport {
	/**
	 * Kwd(KwdPackage)
	 *  |- Const(CIdent(_))
	 *      |- Dot
	 *          |- Const(CIdent(_))
	 *              |- Semicolon
	 *
	 * Kwd(KwdImport)
	 *  |- Const(CIdent(_))
	 *      |- Dot
	 *          |- Const(CIdent(_))
	 *              |- Semicolon
	 *
	 */
	public static function walkPackageImport(stream:TokenStream, parent:TokenTree) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (stream.hasMore() && progress.streamHasChanged()) {
			switch (stream.token()) {
				case Sharp(_):
					WalkSharp.walkSharp(stream, parent, walkPackageImport);
				case Semicolon:
					var newChild:TokenTree = stream.consumeToken();
					parent.addChild(newChild);
					return;
				default:
					var newChild:TokenTree = stream.consumeToken();
					parent.addChild(newChild);
					parent = newChild;
			}
		}
	}
}