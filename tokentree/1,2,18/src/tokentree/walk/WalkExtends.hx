package tokentree.walk;

class WalkExtends {
	public static function walkExtends(stream:TokenStream, parent:TokenTree) {
		if (!stream.tokenForMatch().match(Kwd(KwdExtends))) return;
		var parentType:TokenTree = stream.consumeTokenDef(Kwd(KwdExtends));
		parent.addChild(parentType);
		WalkComment.walkComment(stream, parent);
		WalkTypeNameDef.walkTypeNameDef(stream, parentType);
		WalkComment.walkComment(stream, parent);
		WalkExtends.walkExtends(stream, parent);
		WalkComment.walkComment(stream, parent);
	}
}