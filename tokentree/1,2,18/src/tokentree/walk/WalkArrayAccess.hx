package tokentree.walk;

class WalkArrayAccess {
	public static function walkArrayAccess(stream:TokenStream, parent:TokenTree) {
		var bkOpen:TokenTree = stream.consumeTokenDef(BkOpen);
		parent.addChild(bkOpen);
		stream.applyTempStore(bkOpen);
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case Kwd(KwdFor):
					stream.applyTempStore(bkOpen);
					WalkFor.walkFor(stream, bkOpen);
				case Kwd(KwdWhile):
					stream.applyTempStore(bkOpen);
					WalkWhile.walkWhile(stream, bkOpen);
				case POpen:
					stream.applyTempStore(bkOpen);
					WalkPOpen.walkPOpen(stream, bkOpen);
				case BrOpen:
					stream.applyTempStore(bkOpen);
					WalkBlock.walkBlock(stream, bkOpen);
				case BkOpen:
					WalkArrayAccess.walkArrayAccess(stream, bkOpen);
				case BkClose:
					break;
				case At:
					stream.addToTempStore(WalkAt.walkAt(stream));
				case Kwd(KwdFunction):
					WalkFunction.walkFunction(stream, bkOpen);
				case Comma:
					var comma:TokenTree = stream.consumeToken();
					var child:Null<TokenTree> = bkOpen.getLastChild();
					if (child == null) child = bkOpen;
					child.addChild(comma);
				case Binop(OpArrow):
					var child:Null<TokenTree> = bkOpen.getLastChild();
					if (child == null) child = bkOpen;
					WalkStatement.walkStatement(stream, child);
				default:
					stream.applyTempStore(bkOpen);
					WalkStatement.walkStatement(stream, bkOpen);
			}
		}
		bkOpen.addChild(stream.consumeTokenDef(BkClose));
		if (stream.hasMore()) {
			switch (stream.token()) {
				case BkOpen | Dot | Binop(_) | Const(CIdent("is")):
					WalkStatement.walkStatementContinue(stream, bkOpen);
				default:
			}
		}
	}
}