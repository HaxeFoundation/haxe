package tokentree.walk;

class WalkNew {
	public static function walkNew(stream:TokenStream, parent:TokenTree) {
		var newTok:TokenTree = stream.consumeTokenDef(Kwd(KwdNew));
		parent.addChild(newTok);
		var name:TokenTree = WalkTypeNameDef.walkTypeNameDef(stream, newTok);
		var pOpen:Null<TokenTree> = null;

		WalkComment.walkComment(stream, name);
		switch (stream.token()) {
			case POpen:
				pOpen = WalkPOpen.walkPOpen(stream, name);
			case Sharp(_):
				WalkSharp.walkSharp(stream, parent, WalkStatement.walkStatement);
			default:
		}
		WalkComment.walkComment(stream, name);

		switch (stream.token()) {
			case Dot | Binop(_) | Const(CIdent("is")) | BkOpen:
				if (pOpen != null) {
					WalkStatement.walkStatement(stream, pOpen);
				}
				else {
					WalkStatement.walkStatement(stream, name);
				}
			default:
		}
	}
}