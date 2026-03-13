package tokentree.walk;

class WalkWhile {
	/**
	 * Kwd(KwdWhile)
	 *  |- POpen
	 *  |   |- expression
	 *  |   |- PClose
	 *  |- BrOpen
	 *      |- statement
	 *      |- statement
	 *      |- BrClose
	 *
	 */
	public static function walkWhile(stream:TokenStream, parent:TokenTree) {
		var whileTok:TokenTree = stream.consumeTokenDef(Kwd(KwdWhile));
		parent.addChild(whileTok);
		stream.applyTempStore(whileTok);
		WalkComment.walkComment(stream, whileTok);
		WalkStatement.walkStatement(stream, whileTok);
		WalkComment.walkComment(stream, whileTok);
		WalkBlock.walkBlock(stream, whileTok);
	}
}