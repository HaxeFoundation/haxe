package tokentree.walk;

class WalkAt {
	/**
	 * At
	 *  |- DblDot
	 *      |- Const(CIdent)
	 *          |- POpen
	 *              |- expression
	 *              |- PClose
	 *
	 * At
	 *  |- DblDot
	 *      |- Const(CIdent)
	 *
	 * At
	 *  |- Const(CIdent)
	 *      |- POpen
	 *          |- expression
	 *          |- PClose
	 *
	 * At
	 *  |- Const(CIdent)
	 *
	 */
	public static function walkAt(stream:TokenStream):TokenTree {
		var atTok:TokenTree = stream.consumeTokenDef(At);
		var parent:TokenTree = atTok;
		if (stream.tokenForMatch().match(DblDot)) {
			var dblDot:TokenTree = stream.consumeToken();
			atTok.addChild(dblDot);
			parent = dblDot;
		}
		walkIdent(stream, parent);
		return atTok;
	}

	static function walkIdent(stream:TokenStream, parent:TokenTree) {
		var ident:TokenTree;
		switch (stream.token()) {
			case Const(CIdent(_)):
				ident = stream.consumeConstIdent();
			case Kwd(_):
				ident = stream.consumeToken();
			case Binop(OpIn):
				ident = stream.consumeToken();
			default:
				return;
		}
		parent.addChild(ident);
		switch (stream.token()) {
			case Dot:
				var child:TokenTree = stream.consumeToken();
				ident.addChild(child);
				walkIdent(stream, child);
			case POpen:
				var pOpenPos:Position = stream.getTokenPos();
				if (ident.pos.max == pOpenPos.min) {
					var tempStore:Array<TokenTree> = stream.getTempStore();
					stream.clearTempStore();
					WalkPOpen.walkPOpen(stream, ident, false);
					for (temp in tempStore) {
						stream.addToTempStore(temp);
					}
				}
			default:
		}
	}

	public static function walkAts(stream:TokenStream) {
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case At:
					stream.addToTempStore(WalkAt.walkAt(stream));
				default:
			}
		}
	}
}