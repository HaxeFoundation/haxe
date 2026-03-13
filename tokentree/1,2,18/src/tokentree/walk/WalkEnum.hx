package tokentree.walk;

class WalkEnum {
	public static function walkEnum(stream:TokenStream, parent:TokenTree) {
		var typeTok:TokenTree = stream.consumeToken();
		if (stream.tokenForMatch().match(Kwd(KwdAbstract))) {
			stream.addToTempStore(typeTok);
			WalkAbstract.walkAbstract(stream, parent);
			return;
		}
		parent.addChild(typeTok);
		var name:TokenTree = WalkTypeNameDef.walkTypeNameDef(stream, typeTok);
		// add all comments, annotations
		stream.applyTempStore(name);
		WalkBlock.walkBlock(stream, name);
	}
}