package tokentree.walk;

class WalkTypedef {
	public static function walkTypedef(stream:TokenStream, parent:TokenTree) {
		var typeTok:TokenTree = stream.consumeToken();
		parent.addChild(typeTok);
		var name:TokenTree = WalkTypeNameDef.walkTypeNameDef(stream, typeTok);
		// add all comments, annotations
		stream.applyTempStore(name);
		if (stream.tokenForMatch().match(Binop(OpAssign))) {
			var assign:TokenTree = stream.consumeTokenDef(Binop(OpAssign));
			name.addChild(assign);
			name = assign;
		}
		WalkTypedefBody.walkTypedefBody(stream, name);
	}
}