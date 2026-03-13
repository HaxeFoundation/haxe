package tokentree.walk;

class WalkIf {
	/**
	 * Kwd(KwdIf)
	 *  |- POpen
	 *  |   |- expression
	 *  |   |- PClose
	 *  |- BrOpen
	 *  |   |- statement
	 *  |   |- statement
	 *  |   |- BrClose
	 *  |- Kwd(KwdElse)
	 *      |- BrOpen
	 *          |- statement
	 *          |- statement
	 *          |- BrClose
	 *
	 */
	public static function walkIf(stream:TokenStream, parent:TokenTree) {
		var ifTok:TokenTree = stream.consumeTokenDef(Kwd(KwdIf));
		parent.addChild(ifTok);
		// condition
		stream.applyTempStore(ifTok);
		WalkStatement.walkStatement(stream, ifTok);
		if (stream.tokenForMatch().match(DblDot)) return;
		// if-expr
		WalkBlock.walkBlock(stream, ifTok);
		WalkComment.tryWalkComment(stream, ifTok, Kwd(KwdElse));
		if (stream.tokenForMatch().match(Kwd(KwdElse))) {
			var elseTok:TokenTree = stream.consumeTokenDef(Kwd(KwdElse));
			ifTok.addChild(elseTok);
			// else-expr
			WalkBlock.walkBlock(stream, elseTok);
		}
	}
}