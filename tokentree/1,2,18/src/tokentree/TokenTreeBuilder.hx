package tokentree;

import byte.ByteData;
import haxeparser.Data;
import tokentree.walk.WalkClass;
import tokentree.walk.WalkFile;
import tokentree.walk.WalkStatement;
import tokentree.walk.WalkTypeNameDef;

class TokenTreeBuilder {
	public static function buildTokenTree(tokens:Array<Token>, bytes:ByteData, entryPoint:Null<TokenTreeEntryPoint> = null):TokenTree {
		if (entryPoint == null) {
			entryPoint = TypeLevel;
		}
		return buildTokenTreeFromStream(new TokenStream(tokens, bytes), entryPoint);
	}

	static function buildTokenTreeFromStream(stream:TokenStream, entryPoint:TokenTreeEntryPoint):TokenTree {
		var root:TokenTree = stream.createDummyToken(Root);
		switch (entryPoint) {
			case TypeLevel:
				WalkFile.walkFile(stream, root);
			case FieldLevel:
				WalkClass.walkClassBody(stream, root);
			case ExpressionLevel:
				WalkStatement.walkStatement(stream, root);
			case TypeHintLevel:
				WalkTypeNameDef.walkTypeNameDef(stream, root);
		}
		if (stream.hasMore()) {
			// stream is not empty!
			switch (TokenStream.MODE) {
				case Relaxed:
					var progress:TokenStreamProgress = new TokenStreamProgress(stream);
					while (progress.streamHasChanged()) {
						WalkStatement.walkStatement(stream, root);
					}
					if (stream.hasMore()) {
						throw "invalid token tree structure - found:" + stream.token();
					}
				case Strict:
					throw "invalid token tree structure - found:" + stream.token();
			}
		}
		var tempStore:Array<TokenTree> = stream.getTempStore();
		switch (TokenStream.MODE) {
			case Strict:
				if (tempStore.length != 0) {
					throw "invalid token tree structure - tokens in temp store:" + tempStore.join(", ");
				}
			case Relaxed:
				for (stored in tempStore) {
					root.addChild(stored);
				}
		}
		return root;
	}
}

enum TokenTreeEntryPoint {
	TypeLevel;
	FieldLevel;
	ExpressionLevel;
	TypeHintLevel;
}