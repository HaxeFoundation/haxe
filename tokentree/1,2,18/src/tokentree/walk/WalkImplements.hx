package tokentree.walk;

class WalkImplements {
	public static function walkImplements(stream:TokenStream, parent:TokenTree) {
		if (!stream.tokenForMatch().match(Kwd(KwdImplements))) return;
		var interfacePart:TokenTree = stream.consumeTokenDef(Kwd(KwdImplements));
		parent.addChild(interfacePart);
		WalkComment.walkComment(stream, parent);
		WalkTypeNameDef.walkTypeNameDef(stream, interfacePart);
		WalkComment.walkComment(stream, parent);
		WalkImplements.walkImplements(stream, parent);
		WalkComment.walkComment(stream, parent);
	}
}