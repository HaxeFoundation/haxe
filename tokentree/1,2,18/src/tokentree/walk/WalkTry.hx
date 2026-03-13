package tokentree.walk;

class WalkTry {
	/**
	 * Kwd(KwdTry)
	 *  |- BrOpen
	 *  |   |- statement
	 *  |   |- statement
	 *  |   |- BrClose
	 *  |- Kwd(KwdCatch)
	 *  |   |- BrOpen
	 *  |       |- statement
	 *  |       |- statement
	 *  |       |- BrClose
	 *  |- Kwd(KwdCatch)
	 *      |- BrOpen
	 *          |- statement
	 *          |- statement
	 *          |- BrClose
	 *
	 */
	public static function walkTry(stream:TokenStream, parent:TokenTree) {
		var tryTok:TokenTree = stream.consumeTokenDef(Kwd(KwdTry));
		parent.addChild(tryTok);
		stream.applyTempStore(tryTok);
		WalkBlock.walkBlock(stream, tryTok);

		var currentPos:Int = stream.getStreamIndex();
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		var comments:Array<TokenTree> = [];
		while (stream.hasMore() && progress.streamHasChanged()) {
			switch (stream.token()) {
				case Comment(_), CommentLine(_):
					comments.push(stream.consumeToken());
				case Kwd(KwdCatch):
					for (comment in comments) {
						tryTok.addChild(comment);
					}
					comments = [];
					WalkTry.walkCatch(stream, tryTok);
					currentPos = stream.getStreamIndex();
				default:
					stream.rewindTo(currentPos);
					return;
			}
		}
	}

	/**
	 * Kwd(KwdCatch)
	 *  |- BrOpen
	 *      |- statement
	 *      |- statement
	 *      |- BrClose
	 *
	 */
	static function walkCatch(stream:TokenStream, parent:TokenTree) {
		var catchTok:TokenTree = stream.consumeTokenDef(Kwd(KwdCatch));
		parent.addChild(catchTok);
		WalkPOpen.walkPOpen(stream, catchTok);
		WalkComment.walkComment(stream, catchTok);
		WalkBlock.walkBlock(stream, catchTok);
	}
}