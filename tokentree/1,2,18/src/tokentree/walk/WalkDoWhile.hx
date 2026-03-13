package tokentree.walk;

class WalkDoWhile {
	/**
	 * Kwd(KwdDo)
	 *  |- BrOpen
	 *  |   |- statement
	 *  |   |- statement
	 *  |   |- BrClose
	 *  |- Kwd(KwdWhile)
	 *      |- POpen
	 *      |   |- expression
	 *      |   |- PClose
	 *      |- Semicolon
	 *
	 */
	public static function walkDoWhile(stream:TokenStream, parent:TokenTree) {
		var doTok:TokenTree = stream.consumeTokenDef(Kwd(KwdDo));
		parent.addChild(doTok);
		stream.applyTempStore(doTok);
		WalkComment.walkComment(stream, doTok);
		WalkBlock.walkBlock(stream, doTok);
		var whileTok:TokenTree = stream.consumeTokenDef(Kwd(KwdWhile));
		doTok.addChild(whileTok);
		WalkStatement.walkStatement(stream, whileTok);
		WalkComment.walkComment(stream, whileTok);
		if (stream.tokenForMatch().match(Semicolon)) whileTok.addChild(stream.consumeToken());
	}
}